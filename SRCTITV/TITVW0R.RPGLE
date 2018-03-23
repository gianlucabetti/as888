000100061214      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ZEFIRO GROUP Cod. 0433659)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700061214     Ftitvw0p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900061214     Ftitvw0ps  O    f  198        PRINTER usropn
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
009500061214     C                   if        not %open(titvw0ps)
009600061214     C                   open      titvw0ps
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
011400061214     C                   if        %open(titvw0ps)
011500000616     C                   except    findett
011600061214     C                   close     titvw0ps
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
013500061214     C                   if        not %open(titvw0p)
013600061214     C                   open      titvw0p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100061214     C                   if        %open(titvw0p)
014200061214     C                   close     titvw0p
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
018800060901     C                   exsr      inzvar
018900060901     C                   exsr      defcam
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
030300061214     C                   EVAL      VABCCM = 0433659
030400061214     C                   EVAL      VABLNP = 043
030500061214     C                   EVAL      VABCTR = 300
030600061003     C                   EVAL      VABCBO = '1'
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
033900060720     C*
034000060720     C* Reperimento campi NUMERICI
034100060720     C                   MOVEL     DATCOR        VABAAS
034200060720     C                   MOVE      DATCOR        VABMGS
034300010607     C*
034400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034500061214     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
034600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034800020117     C* ==
034900061214     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
035000061214     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
035100061214     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
035200061214     C                   IF        %subst(vindta:207:1) = 'F'
035300061214     C                   EVAL      VABCBO='1'
035400061214     C                   ENDIF
035500061214     C                   IF        %subst(vindta:207:1) = 'A'
035600061214     C                   EVAL      VABCBO='2'
035700061214     C                   ENDIF
035800061214     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
035900061214     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
036000061214     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
036100060720     C* CAD
036200061214     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
036300060720     C                   EXSR      CHKNUM
036400060720     C                   IF        PiInt=*on
036500060720     C                   Z-ADD     PiVal         Num5_0
036600060720     C                   MOVEL(p)  Num5_0        VABCAD
036700060720     C                   ELSE
036800060720     C                   SETON                                        32
036900060720     C                   EVAL      VABCAD = *zeros
037000060720     C                   EVAL      vinmsg = %trimr(vinmsg)
037100060720     C                             + ' ' + 'VABCAD'
037200060720     C                   ENDIF
037300061017     C* NSP
037400061214     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
037500061017     C                   EXSR      CHKNUM
037600061017     C                   IF        PiInt=*on
037700061017     C                   Z-ADD     PiVal         VABNSP
037800061017     C                   ELSE
037900061017     C                   SETON                                        31
038000061017     C                   Z-ADD     *zeros        VABNSP
038100061017     C                   EVAL      vinmsg = %trimr(vinmsg)
038200061017     C                             + ' ' + 'VABNSP'
038300061017     C                   ENDIF
038400061017     C* RMN
038500061214     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
038600020304     C                   EXSR      CHKNUM
038700020304     C                   IF        PiInt=*on
038800040301     C                   Z-ADD     PiVal         VABRMN
038900020304     C                   ELSE
039000061017     C                   SETON                                        32
039100040301     C                   Z-ADD     *zeros        VABRMN
039200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
039300061017     C                             + ' ' + 'VABRMN'
039400020304     C                   ENDIF
039500040420     C* NCL
039600061214     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
039700010607     C                   EXSR      CHKNUM
039800010607     C                   IF        PiInt=*on
039900010607     C                   Z-ADD     PiVal         VABNCL
040000010607     C                   ELSE
040100010607     C                   SETON                                        32
040200010607     C                   Z-ADD     *zeros        VABNCL
040300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040400010607     C                             + ' ' + 'VABNCL'
040500010607     C                   ENDIF
040600040420     C* PKB
040700061214     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
040800010607     C                   EXSR      CHKNUM
040900010607     C                   IF        PiNum=*on
041000061214     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
041100040420     C                   Z-ADD     PiVal         VABPKB
041200010607     C                   ELSE
041300010607     C                   SETON                                        32
041400010607     C                   Z-ADD     *zeros        VABPKB
041500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041600010607     C                             + ' ' + 'VABPKB'
041700010607     C                   ENDIF
041800061214 xxx C* VLB
041900061214     C                   EVAL      PiStr=%trim(%subst(vindta:234:11))
042000061214     C                   EXSR      CHKNUM
042100061214     C                   IF        PiNum=*on
042200061214     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
042300061214     C                   Z-ADD     PiVal         VABVLB
042400061214     C                   ELSE
042500061214     C                   SETON                                        32
042600061214     C                   Z-ADD     *zeros        VABVLB
042700061214     C                   EVAL      vinmsg = %trimr(vinmsg)
042800061214     C                             + ' ' + 'VABVLB'
042900061214     C                   ENDIF
043000060901     C* CAS
043100061214     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
043200061003     C                   EXSR      CHKNUM
043300060720     C                   IF        PiNum=*on
043400061214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
043500060901     C                   Z-ADD     PiVal         VABCAS
043600061003     C                   IF        VABCAS > 0
043700061003     C                   EVAL      FlgCAS = '1'
043800061003     C                   EVAL      VABVCA =  'EUR'
043900061003     C                   ENDIF
044000060720     C                   ELSE
044100060720     C                   SETON                                        32
044200060901     C                   Z-ADD     *zeros        VABCAS
044300060720     C                   EVAL      vinmsg = %trimr(vinmsg)
044400060901     C                             + ' ' + 'VABCAS'
044500060720     C                   ENDIF
044600061214     C* IAS
044700061214     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
044800061214     C                   EXSR      CHKNUM
044900061214     C                   IF        PiNum=*on
045000061214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
045100061214     C                   Z-ADD     PiVal         VABIAS
045200061214     C                   IF        VABIAS > 0
045300061214     C                   EVAL      VABVAS =  'EUR'
045400061214     C                   ENDIF
045500061214     C                   ELSE
045600061214     C                   SETON                                        32
045700061214     C                   Z-ADD     *zeros        VABIAS
045800061214     C                   EVAL      vinmsg = %trimr(vinmsg)
045900061214     C                             + ' ' + 'VABIAS'
046000061214     C                   ENDIF
046100020322     C*
046200020322     C* Considerazioni finali su CBO/CAS
046300061017     C                   IF        FlgCAS = '1'
046400061017     C                   IF        VABCBO = '1'
046500061017     C                   EVAL      VABCBO = '4'
046600061017     C                   ENDIF
046700061017     C                   IF        VABCBO = '2'
046800061017     C                   EVAL      VABCBO = '6'
046900061017     C                   ENDIF
047000061017     C                   ENDIF
047100020204     C*
047200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047300020204     C                   EXSR      CHKIMPDIV
047400020204     C*
047500010607     C                   ENDSR
047600010607     C*----------------------------------------------------*
047700010601
047800020204
047900020204     C*----------------------------------------------------*
048000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048100020204     C*----------------------------------------------------*
048200020204     C     CHKIMPDIV     BEGSR
048300020204     C*
048400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048500020204     C                   Z-ADD     *zeros        wrkDec            9 9
048600020204     C*
048700020204     C* Come prima cosa effettuo considerazioni sulla divisa
048800020204     C                   IF        vabIAS > *zeros
048900020204     C                   IF        vabVAS <> 'EUR'
049000020204     C                   EVAL      vabVAS =  'ITL'
049100020204     C                   ENDIF
049200020204     C                   ENDIF
049300020204     C*
049400020204     C                   IF        vabCAS > *zeros
049500020204     C                   IF        vabVCA <> 'EUR'
049600020204     C                   EVAL      vabVCA =  'ITL'
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C                   IF        vabVMD > *zeros
050100020321     C                   IF        vabVAD <> 'EUR'
050200020204     C                   EVAL      vabVAD =  'ITL'
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050700020204     C                   Z-ADD     vabIAS        wrkDec
050800020204     C                   IF        wrkDec > *zeros
050900020204     C                   IF        vabVAS = 'ITL'
051000020204     C                   EVAL      vabIAS = *zeros
051100020204     C                   ENDIF
051200020204     C                   ENDIF
051300020204     C*
051400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
051500020204     C                   Z-ADD     vabCAS        wrkDec
051600020204     C                   IF        wrkDec > *zeros
051700020204     C                   IF        vabVCA = 'ITL'
051800020204     C                   EVAL      vabCAS = *zeros
051900020204     C                   ENDIF
052000020204     C                   ENDIF
052100020204     C*
052200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052300020204     C                   Z-ADD     vabVMD        wrkDec
052400020204     C                   IF        wrkDec > *zeros
052500020204     C                   IF        vabVAD = 'ITL'
052600020204     C                   EVAL      vabVMD = *zeros
052700020204     C                   ENDIF
052800020204     C                   ENDIF
052900020204     C*
053000020204     C                   ENDSR
053100020204     C***
053200020204
053300010330
053400010330
053500010330     C*----------------------------------------------------*
053600010330     C*  CONTROLLO NUMERICITA' CAMPI
053700010330     C*----------------------------------------------------*
053800010330     C     CHKNUM        BEGSR
053900010330     C*
054000010606     C                   IF        PiDecChr = *blanks
054100061017     C                   EVAL      PiDecChr = '.'
054200010606     C                   ENDIF
054300010606     C*
054400010606     C                   CALL(e)   'ISNUMERIC'
054500010330     C                   PARM                    PiStr            30
054600010606     C                   PARM                    PiDecChr          1
054700010330     C                   PARM      *ZEROS        PiVal            30 9
054800010330     C                   PARM      '0'           PiInt             1
054900010330     C                   PARM      '0'           PiNum             1
055000010330     C                   IF        %error
055100010606     C                   EVAL      PiNum=*off
055200010330     C                   ENDIF
055300010330     C*
055400010330     C                   ENDSR
055500010330     C***
055600010330
055700010601
055800010601
055900010601
056000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056100010601     C     repfil        BEGSR
056200010601     C*
056300010601     C                   if        invfil = *zeros and
056400010601     C                             depfil > *zeros and
056500010629     C                             (vinflg = *blanks or
056600010629     C                              vinflg = *zeros)
056700010601     C
056800010601     C                   eval      invfil = depfil
056900010601     C                   endif
057000010601     C*
057100010601     C                   if        depfil <> invfil and
057200010601     C                             invfil > *zeros
057300010601     C                   eval      flgMulti = '1'
057400010601     C                   if        vinflg = *blanks
057500010601     C                   add       1             cntNonEl
057600010601     C                   endif
057700010601     C                   endif
057800010601     C*
057900010601     C                   if        vinflg = '2'
058000010601     C                   eval      flgStato = '2'
058100010601     C                   endif
058200010601     C*
058300010601     C                   ENDSR
058400010601     C***
058500010601
058600010601
058700010601
058800010330
058900010330
059000010330
059100990920      /TITLE Invio dei dati al punto operativo.
059200000613     C     invio         BEGSR
059300990920     C*
059400990920     C                   reset                   dscmz
059500010601     C                   move      invfil        cmzdst
059600990920     C                   eval      cmzfld = vlrfou
059700990920     C                   eval      cmzmbd = vlrhdl
059800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
059900000710     C                   if        prmfir = *blanks
060000021014     C                   eval      cmzfla = 'FIVAB00F'
060100021014     C                   eval      cmzmba = 'FIVAB00F'
060200000710     C                   else
060300000710     C                   eval      cmzfla = prmfir
060400000710     C                   eval      cmzmba = prmfir
060500000710     C                   endif
060600990920     C                   eval      cmznrr = *zeros
060700990920     C                   move      §ctrok        cmznrr
060800021018     C                   eval      cmzlba = vlrfl1
060900990920     C                   call(e)   'TIS711C'
061000990920     C                   parm                    dscmz
061100990921     C                   parm      *blanks       esito
061200990920     C                   if        %error
061300990920     C                             or cmzerr = '1'
061400990921     C                             or esito  = '1'
061500000710     C                   eval      wrkesito = '3'
061600990920     C                   endif
061700990920     C*
061800000613     C                   ENDSR
061900990910
062000010601
062100010601
062200010601
062300010601
062400010601      /TITLE Invio dei dati al punto operativo.
062500010601     C     opeini        BEGSR
062600010601     C*
062700010601     C* Inizializzo flag e contatori operativi
062800010601     C                   movel     '0'           flgGiro           1
062900010601     C                   movel     '0'           flgMulti          1
063000010601     C                   movel     '1'           flgStato          1
063100010615     C                   movel     '0'           flgOk             1
063200010601     C                   z-add     *zeros        cntNonEl         10 0
063300010601     C                   z-add     *zeros        depfil            3 0
063400010601     C                   z-add     *zeros        invfil            3 0
063500010601     C*
063600010601     C                   ENDSR
063700010601     C***
063800010601
063900010601
064000010601
064100010330
064200010330
064300000613     C     *inzsr        BEGSR
064400990910     C*
064500990910     C     *entry        plist
064600990920     C                   parm                    tivlrds
064700990921     C                   parm      wrkesito      esito
064800000724     C                   parm                    prmlit
064900000710     C                   parm                    prmfir
065000010330     C*
065100010330     C* CALCOLA LA DATA CORRENTE
065200010330     C                   time                    wn14             14 0
065300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065400010330     C                   z-add     wn8           g08dat
065500010330     C                   z-add     *zeros        g08inv
065600010330     C                   movel     '0'           g08err
065700010330     C                   call      'XSRDA8'
065800010330     C                   parm                    wlbda8
065900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066000000613     C*
066100000613     C                   ENDSR
066200000613     C***
066300990908
066400061214     Otitvw0p   E            riepilogo         2
066500990915     O                                              'Upload via Internet'
066600990915     O                                           +1 'Traduzione TIVIN00R -'
066700021014     O                                           +1 'FIVABWWR'
066800990915     O                                           +1 'Testate bolle'
066900990915     O          E            riepilogo   2
067000990915     O                       wrkdata
067100990915     O                       wrkora              +1
067200990915     O                       procname            +1
067300990915     O          E            riepilogo   2
067400990915     O                                              'Cliente..................:'
067500010605     O                       VABCCM        z     +1
067600990915     O          E            riepilogo   2
067700990920     O                                              'Riferimento Strategi.....:'
067800990920     O                       vlrhdl              +1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Giusti...................:'
068100971022     O                       §CTROK        2   +  1
068200990915     O          E            riepilogo   2
068300990915     O                                              'Sbagliati e corretti.....:'
068400971022     O                       §CTRMO        2   +  1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Sbagliati e scartati.....:'
068700971022     O                       §CTRNO        2   +  1
068800000613
068900061214     Otitvw0ps  E            testdett          2
069000000613     O                                              'Upload via Internet'
069100000613     O                                           +1 'Traduzione TIVIN00R -'
069200021014     O                                           +1 'FIVABWWR'
069300000613     O                                           +1 'Testate bolle'
069400000616     O          E            testdett    3
069500000613     O                                           +2 'N° rec'
069600000613     O                                           +3 'Anteprima contenuto'
069700000616     O          E            rigadett    2
069800000613     O                       rrnum               +2
069900000621     O                       recko               +3
070000000616     O          E            findett     2
070100000613     O                       wrkdata
070200000613     O                       wrkora              +1
070300000613     O                       procname            +1
070400000616     O          E            findett     2
070500000613     O                                              'Cliente..................:'
070600010605     O                       VABCCM        z     +1
070700000616     O          E            findett     2
070800000613     O                                              'Riferimento Strategi.....:'
070900000613     O                       vlrhdl              +1
071000000616     O          E            findett     2
071100000613     O                                              'Giusti...................:'
071200000613     O                       §CTROK        2   +  1
071300000616     O          E            findett     2
071400000613     O                                              'Sbagliati e corretti.....:'
071500000613     O                       §CTRMO        2   +  1
071600000616     O          E            findett     2
071700000613     O                                              'Sbagliati e scartati.....:'
071800000613     O                       §CTRNO        2   +  1
071900000613** CMD - COMANDI CL
072000061214OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVW0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
072100061214OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVW0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
072200061214DLTOVR FILE(TITVW0P TITVW0PS) LVL(*)
072300010606
072400000613
