000100050603      /TITLE Upload via Internet: traduzione in EDIVABWR. (x SINERGY SOLUTION Cod. 2493358)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050603     FEDIVABwr  O    E             DISK    usropn
000700050119     Ftitvg9p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050119     Ftitvg9ps  O    f  198        PRINTER usropn
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
009400050119     C                   if        not %open(titvg9ps)
009500050119     C                   open      titvg9ps
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
011300050119     C                   if        %open(titvg9ps)
011400000616     C                   except    findett
011500050119     C                   close     titvg9ps
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
013400050119     C                   if        not %open(titvg9p)
013500050119     C                   open      titvg9p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000050119     C                   if        %open(titvg9p)
014100050119     C                   close     titvg9p
014200990914     C                   endif
014300000613     C*
014400000613     C                   ENDSR
014500000613     C***
014600000613
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000050603     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700050603     C                   if        not %open(edivabwr)
015800050603     C                   open      edivabwr
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
020300050603     C*
020400050603     C* Valorizzo i campi relativi al CMR
020500050603     C                   MOVEL     datoracor     VABCMR
020600050603     C                   EVAL      VABDCM = datcor
020700050603     C                   EVAL      VABDTS = datcor
020800050603     C                   EVAL      VABHMS = oracor
020900050603     C                   EVAL      VABCNT = 1
021000010601     C*
021100010604     C* Ebbene...
021200010604     C*
021300010604     C  N31              ADD       1             §CTROK            7 0
021400010604     C   32              ADD       1             §CTRMO            7 0
021500010604     C   31              ADD       1             §CTRNO            7 0
021600020205     C*
021700050603     C* Scarico il buffer del EDIVAB
021800050603     C  N31              WRITE     EDIVAB00
021900010604     C*
022000020722     C                   endif
022100020722     C*
022200010604     C                   if        *in31 = *off and
022300010604     C                             *in32 = *off
022400010604     C                   eval      vinflg = '1'
022500010604     C                   else
022600010604     C                   eval      recko = vindta
022700010604     C                   except    rigadett
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700050603     C                   if        %open(edivabwr)
026800050603     C                   close     edivabwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010330
028700010601
028800010601
028900010601
029000010601
029100010330     C*----------------------------------------------------*
029200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300010330     C*----------------------------------------------------*
029400010330     C     INZVAR        BEGSR
029500010330     C*
029600020204     C                   Z-ADD     *zeros        Num5_0            5 0
029700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029800020322     C                   MOVEL     '0'           FlgCAS            1
029900010330     C*
030000010330     C                   ENDSR
030100010330     C*----------------------------------------------------*
030200020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030300010330     C*----------------------------------------------------*
030400010330     C     DEFCAM        BEGSR
030500010330     C*
030600020204     C* Inizializzo il buffer del record da scrivere
030700050603     C                   CLEAR                   EDIVAB00
030800020204     C* Imposto i valori di default...
030900050119     C                   EVAL      VABCCM = 2493358
031000050119     C                   EVAL      VABLNP = 249
031100040713     C                   EVAL      VABCTR = 000
031200041108     C                   EVAL      VABCBO = '1'
031300020204     C* ... e poi verifico se sono stati passati come parametri
031400020204     C                   IF        vlrppt > *blanks
031500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABCCM
032000020204     C                   ENDIF
032100040301     C                   ENDIF
032200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABLNP
032700020204     C                   ENDIF
032800040301     C                   ENDIF
032900040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033100020204     C                   EXSR      CHKNUM
033200020204     C                   IF        PiInt=*on
033300020204     C                   Z-ADD     PiVal         VABCTR
033400040301     C                   ENDIF
033500020204     C                   ENDIF
033600020204     C                   ENDIF
033700020204     C*
033800010330     C                   ENDSR
033900010607     C*----------------------------------------------------*
034000050603     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
034100010607     C*----------------------------------------------------*
034200010607     C     IMPVAB        BEGSR
034300010607     C*
034400010607     C                   SETOFF                                       3132
034500010607     C*
034600010607     C* Reperimento campi ALFA
034700010607     C*
034800010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034900050119     C                   EVAL      VABRSD=%trim(%subst(vindta:282:35))
035000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035200020117     C* ==
035300050119     C                   EVAL      VABRD2=%trim(%subst(vindta:317:35))
035400050119     C                   EVAL      VABIND=%trim(%subst(vindta:352:35))
035500050119     C                   EVAL      VABLOD=%trim(%subst(vindta:387:35))
035600050119     C                   EVAL      VABPRD=%trim(%subst(vindta:456:2))
035700051117     C                   EVAL      VABNOT=%trim(%subst(vindta:882:35))
035800051117     C                   EVAL      VABNT2=%trim(%subst(vindta:882+35:41-35))
035900050119     C                   IF        %subst(vindta:458:2) <> 'IT'
036000050119     C                   EVAL      VABNZD=%trim(%subst(vindta:458:2))
036100050119     C                   ENDIF
036200050119     C                   EVAL      VABRMA=%trim(%subst(vindta:214:15))
036300050119     C                   EVAL      VABNAS=%trim(%subst(vindta:475:20))
036400050119     C                   IF        %subst(vindta:623:1) = 'A'
036500050119     C                   EVAL      VABCBO='2'
036600050119     C                   ENDIF
036700010607     C*
036800010607     C* Reperimento campi NUMERICI
036900020722     C                   MOVEL     DATCOR        VABAAS
037000020722     C                   MOVE      DATCOR        VABMGS
037100040420     C* NSP/RMN
037200050119     C                   EVAL      PiStr=%trim(%subst(vindta:229:15))
037300020304     C                   EXSR      CHKNUM
037400020304     C                   IF        PiInt=*on
037500040301     C                   Z-ADD     PiVal         VABNSP
037600040301     C                   Z-ADD     PiVal         VABRMN
037700020304     C                   ELSE
037800020722     C                   SETON                                        31
037900020304     C                   Z-ADD     *zeros        VABNSP
038000040301     C                   Z-ADD     *zeros        VABRMN
038100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038200040301     C                             + ' ' + 'VABNSP VABRMN'
038300020304     C                   ENDIF
038400040420     C* CAD
038500050119     C                   EVAL      PiStr=%trim(%subst(vindta:449:7))
038600010607     C                   EXSR      CHKNUM
038700010607     C                   IF        PiInt=*on
038800020204     C                   Z-ADD     PiVal         Num5_0
038900020204     C                   MOVEL(p)  Num5_0        VABCAD
039000010607     C                   ELSE
039100010607     C                   SETON                                        32
039200020204     C                   EVAL      VABCAD = *zeros
039300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039400020204     C                             + ' ' + 'VABCAD'
039500010607     C                   ENDIF
039600040420     C* NCL
039700050119     C                   EVAL      PiStr=%trim(%subst(vindta:505:5))
039800010607     C                   EXSR      CHKNUM
039900010607     C                   IF        PiInt=*on
040000010607     C                   Z-ADD     PiVal         VABNCL
040100010607     C                   ELSE
040200010607     C                   SETON                                        32
040300010607     C                   Z-ADD     *zeros        VABNCL
040400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040500010607     C                             + ' ' + 'VABNCL'
040600010607     C                   ENDIF
040700040420     C* PKB
040800050119     C                   EVAL      PiStr=%trim(%subst(vindta:510:7))
040900010607     C                   EXSR      CHKNUM
041000010607     C                   IF        PiNum=*on
041100050119     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
041200050114     C                   Z-ADD     PiVal         VABPKB
041300010607     C                   ELSE
041400010607     C                   SETON                                        32
041500010607     C                   Z-ADD     *zeros        VABPKB
041600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041700010607     C                             + ' ' + 'VABPKB'
041800010607     C                   ENDIF
041900041103     C* CAS
042000050119     C                   IF        %subst(vindta:594:13) <> *all'0'
042100050119     C                   MOVEL     '1'           FlgCAS
042200060714     C                   EVAL      PiStr=%trim(%subst(vindta:594:15))
042300041103     C                   EXSR      CHKNUM
042400041103     C                   IF        PiNum=*on
042500050119     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
042600050114     C                   Z-ADD     PiVal         VABCAS
042700050119     C                   EVAL      VABVCA = %trim(%subst(vindta:541:4))
042800060714     C                   IF        VABVCA = *blanks
042900060714     C                   EVAL      VABVCA = 'EUR'
043000060714     C                   ENDIF
043100041103     C                   ELSE
043200041103     C                   SETON                                        32
043300041103     C                   Z-ADD     *zeros        VABCAS
043400041103     C                   EVAL      vinmsg = %trimr(vinmsg)
043500041103     C                             + ' ' + 'VABCAS'
043600041103     C                   ENDIF
043700050119     C                   ENDIF
043800020322     C*
043900020322     C* Considerazioni finali su CBO/CAS
044000020322     C                   IF        FlgCAS = '1'
044100040713     C                   IF        VABCBO = '1'
044200020322     C                   EVAL      VABCBO = '4'
044300040713     C                   ENDIF
044400040713     C                   IF        VABCBO = '2'
044500040713     C                   EVAL      VABCBO = '6'
044600040713     C                   ENDIF
044700020322     C                   ENDIF
044800020204     C*
044900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045000020204     C                   EXSR      CHKIMPDIV
045100020204     C*
045200010607     C                   ENDSR
045300010607     C*----------------------------------------------------*
045400010601
045500020204
045600020204     C*----------------------------------------------------*
045700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045800020204     C*----------------------------------------------------*
045900020204     C     CHKIMPDIV     BEGSR
046000020204     C*
046100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046200020204     C                   Z-ADD     *zeros        wrkDec            9 9
046300020204     C*
046400020204     C* Come prima cosa effettuo considerazioni sulla divisa
046500020204     C                   IF        vabIAS > *zeros
046600020204     C                   IF        vabVAS <> 'EUR'
046700020204     C                   EVAL      vabVAS =  'ITL'
046800020204     C                   ENDIF
046900020204     C                   ENDIF
047000020204     C*
047100020204     C                   IF        vabCAS > *zeros
047200020204     C                   IF        vabVCA <> 'EUR'
047300020204     C                   EVAL      vabVCA =  'ITL'
047400020204     C                   ENDIF
047500020204     C                   ENDIF
047600020204     C*
047700020204     C                   IF        vabVMD > *zeros
047800020321     C                   IF        vabVAD <> 'EUR'
047900020204     C                   EVAL      vabVAD =  'ITL'
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048400020204     C                   Z-ADD     vabIAS        wrkDec
048500020204     C                   IF        wrkDec > *zeros
048600020204     C                   IF        vabVAS = 'ITL'
048700020204     C                   EVAL      vabIAS = *zeros
048800020204     C                   ENDIF
048900020204     C                   ENDIF
049000020204     C*
049100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049200020204     C                   Z-ADD     vabCAS        wrkDec
049300020204     C                   IF        wrkDec > *zeros
049400020204     C                   IF        vabVCA = 'ITL'
049500020204     C                   EVAL      vabCAS = *zeros
049600020204     C                   ENDIF
049700020204     C                   ENDIF
049800020204     C*
049900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050000020204     C                   Z-ADD     vabVMD        wrkDec
050100020204     C                   IF        wrkDec > *zeros
050200020204     C                   IF        vabVAD = 'ITL'
050300020204     C                   EVAL      vabVMD = *zeros
050400020204     C                   ENDIF
050500020204     C                   ENDIF
050600020204     C*
050700020204     C                   ENDSR
050800020204     C***
050900020204
051000010330
051100010330
051200010330     C*----------------------------------------------------*
051300010330     C*  CONTROLLO NUMERICITA' CAMPI
051400010330     C*----------------------------------------------------*
051500010330     C     CHKNUM        BEGSR
051600010330     C*
051700010606     C                   IF        PiDecChr = *blanks
051800010606     C                   EVAL      PiDecChr = ','
051900010606     C                   ENDIF
052000010606     C*
052100010606     C                   CALL(e)   'ISNUMERIC'
052200010330     C                   PARM                    PiStr            30
052300010606     C                   PARM                    PiDecChr          1
052400010330     C                   PARM      *ZEROS        PiVal            30 9
052500010330     C                   PARM      '0'           PiInt             1
052600010330     C                   PARM      '0'           PiNum             1
052700010330     C                   IF        %error
052800010606     C                   EVAL      PiNum=*off
052900010330     C                   ENDIF
053000010330     C*
053100010330     C                   ENDSR
053200010330     C***
053300010330
053400010601
053500010601
053600010601
053700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053800010601     C     repfil        BEGSR
053900010601     C*
054000010601     C                   if        invfil = *zeros and
054100010601     C                             depfil > *zeros and
054200010629     C                             (vinflg = *blanks or
054300010629     C                              vinflg = *zeros)
054400010601     C
054500010601     C                   eval      invfil = depfil
054600010601     C                   endif
054700010601     C*
054800010601     C                   if        depfil <> invfil and
054900010601     C                             invfil > *zeros
055000010601     C                   eval      flgMulti = '1'
055100010601     C                   if        vinflg = *blanks
055200010601     C                   add       1             cntNonEl
055300010601     C                   endif
055400010601     C                   endif
055500010601     C*
055600010601     C                   if        vinflg = '2'
055700010601     C                   eval      flgStato = '2'
055800010601     C                   endif
055900010601     C*
056000010601     C                   ENDSR
056100010601     C***
056200010601
056300010601
056400010601
056500010330
056600010330
056700010330
056800990920      /TITLE Invio dei dati al punto operativo.
056900000613     C     invio         BEGSR
057000990920     C*
057100990920     C                   reset                   dscmz
057200010601     C                   move      invfil        cmzdst
057300990920     C                   eval      cmzfld = vlrfou
057400990920     C                   eval      cmzmbd = vlrhdl
057500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057600000710     C                   if        prmfir = *blanks
057700050603     C                   eval      cmzfla = 'EDIVAB0F'
057800050603     C                   eval      cmzmba = 'EDIVAB0F'
057900000710     C                   else
058000000710     C                   eval      cmzfla = prmfir
058100000710     C                   eval      cmzmba = prmfir
058200000710     C                   endif
058300990920     C                   eval      cmznrr = *zeros
058400990920     C                   move      §ctrok        cmznrr
058500021018     C                   eval      cmzlba = vlrfl1
058600990920     C                   call(e)   'TIS711C'
058700990920     C                   parm                    dscmz
058800990921     C                   parm      *blanks       esito
058900990920     C                   if        %error
059000990920     C                             or cmzerr = '1'
059100990921     C                             or esito  = '1'
059200000710     C                   eval      wrkesito = '3'
059300990920     C                   endif
059400990920     C*
059500000613     C                   ENDSR
059600990910
059700010601
059800010601
059900010601
060000010601
060100010601      /TITLE Invio dei dati al punto operativo.
060200010601     C     opeini        BEGSR
060300010601     C*
060400010601     C* Inizializzo flag e contatori operativi
060500010601     C                   movel     '0'           flgGiro           1
060600010601     C                   movel     '0'           flgMulti          1
060700010601     C                   movel     '1'           flgStato          1
060800010615     C                   movel     '0'           flgOk             1
060900010601     C                   z-add     *zeros        cntNonEl         10 0
061000010601     C                   z-add     *zeros        depfil            3 0
061100010601     C                   z-add     *zeros        invfil            3 0
061200010601     C*
061300010601     C                   ENDSR
061400010601     C***
061500010601
061600010601
061700010601
061800010330
061900010330
062000000613     C     *inzsr        BEGSR
062100990910     C*
062200990910     C     *entry        plist
062300990920     C                   parm                    tivlrds
062400990921     C                   parm      wrkesito      esito
062500000724     C                   parm                    prmlit
062600000710     C                   parm                    prmfir
062700010330     C*
062800010330     C* CALCOLA LA DATA CORRENTE
062900010330     C                   time                    wn14             14 0
063000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063100050603     C                   movel     wn14          oracor            6 0          *ORA  (8) IN HH/MM/S
063200010330     C                   z-add     wn8           g08dat
063300010330     C                   z-add     *zeros        g08inv
063400010330     C                   movel     '0'           g08err
063500010330     C                   call      'XSRDA8'
063600010330     C                   parm                    wlbda8
063700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063800050603     C                   movel     datcor        datoracor        14 0          *DATA/ORA CORRENTE
063900050603     C                   move      oracor        datoracor
064000000613     C*
064100000613     C                   ENDSR
064200000613     C***
064300990908
064400050119     Otitvg9p   E            riepilogo         2
064500990915     O                                              'Upload via Internet'
064600990915     O                                           +1 'Traduzione TIVIN00R -'
064700050603     O                                           +1 'EDIVABWR'
064800990915     O                                           +1 'Testate bolle'
064900990915     O          E            riepilogo   2
065000990915     O                       wrkdata
065100990915     O                       wrkora              +1
065200990915     O                       procname            +1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Cliente..................:'
065500010605     O                       VABCCM        z     +1
065600990915     O          E            riepilogo   2
065700990920     O                                              'Riferimento Strategi.....:'
065800990920     O                       vlrhdl              +1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Giusti...................:'
066100971022     O                       §CTROK        2   +  1
066200990915     O          E            riepilogo   2
066300990915     O                                              'Sbagliati e corretti.....:'
066400971022     O                       §CTRMO        2   +  1
066500990915     O          E            riepilogo   2
066600990915     O                                              'Sbagliati e scartati.....:'
066700971022     O                       §CTRNO        2   +  1
066800000613
066900050119     Otitvg9ps  E            testdett          2
067000000613     O                                              'Upload via Internet'
067100000613     O                                           +1 'Traduzione TIVIN00R -'
067200050603     O                                           +1 'EDIVABWR'
067300000613     O                                           +1 'Testate bolle'
067400000616     O          E            testdett    3
067500000613     O                                           +2 'N° rec'
067600000613     O                                           +3 'Anteprima contenuto'
067700000616     O          E            rigadett    2
067800000613     O                       rrnum               +2
067900000621     O                       recko               +3
068000000616     O          E            findett     2
068100000613     O                       wrkdata
068200000613     O                       wrkora              +1
068300000613     O                       procname            +1
068400000616     O          E            findett     2
068500000613     O                                              'Cliente..................:'
068600010605     O                       VABCCM        z     +1
068700000616     O          E            findett     2
068800000613     O                                              'Riferimento Strategi.....:'
068900000613     O                       vlrhdl              +1
069000000616     O          E            findett     2
069100000613     O                                              'Giusti...................:'
069200000613     O                       §CTROK        2   +  1
069300000616     O          E            findett     2
069400000613     O                                              'Sbagliati e corretti.....:'
069500000613     O                       §CTRMO        2   +  1
069600000616     O          E            findett     2
069700000613     O                                              'Sbagliati e scartati.....:'
069800000613     O                       §CTRNO        2   +  1
069900000613** CMD - COMANDI CL
070000050119OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070100050119OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070200050119DLTOVR FILE(TITVG9P TITVG9PS) LVL(*)
070300010606
070400000613
