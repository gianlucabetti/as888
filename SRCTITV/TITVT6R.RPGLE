000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060629     Ftitvt6p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060629     Ftitvt6ps  O    f  198        PRINTER usropn
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
009300060629     C                   if        not %open(titvt6ps)
009400060629     C                   open      titvt6ps
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
011200060629     C                   if        %open(titvt6ps)
011300000616     C                   except    findett
011400060629     C                   close     titvt6ps
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
013300060629     C                   if        not %open(titvt6p)
013400060629     C                   open      titvt6p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060629     C                   if        %open(titvt6p)
014000060629     C                   close     titvt6p
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
030000080715     C                   EVAL      VABCCM = 0434359
030100060629     C                   EVAL      VABLNP = 277
030200080715     C                   EVAL      VABCTR = 000
030300020204     C* ... e poi verifico se sono stati passati come parametri
030400020204     C                   IF        vlrppt > *blanks
030500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700020204     C                   EXSR      CHKNUM
030800020204     C                   IF        PiInt=*on
030900020204     C                   Z-ADD     PiVal         VABCCM
031000020204     C                   ENDIF
031100040301     C                   ENDIF
031200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         VABLNP
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCTR
032400040301     C                   ENDIF
032500020204     C                   ENDIF
032600020204     C                   ENDIF
032700020204     C*
032800010330     C                   ENDSR
032900010607     C*----------------------------------------------------*
033000050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033100010607     C*----------------------------------------------------*
033200010607     C     IMPVAB        BEGSR
033300010607     C*
033400010607     C                   SETOFF                                       3132
033500010607     C*
033600010607     C* Reperimento campi ALFA
033700010607     C*
033800010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
033900060629     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
034000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034200020117     C* ==
034300060629     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
034400060629     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
034500080715     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
034600061005     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
034700060629     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
034800060629     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
034900060630     C                   IF        %trim(%subst(vindta:207:1)) = 'A'
035000060630     C                   EVAL      VABCBO='2'
035100060629     C                   ELSE
035200060630     C                   EVAL      VABCBO='1'
035300060629     C                   ENDIF
035400060510     C*
035500060510     C* Reperimento campi NUMERICI
035600060510     C                   MOVEL     DATCOR        VABAAS
035700060510     C                   MOVE      DATCOR        VABMGS
035800060926     C* NSP
035900060629     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
036000060510     C                   EXSR      CHKNUM
036100060510     C                   IF        PiInt=*on
036200060510     C                   Z-ADD     PiVal         VABNSP
036300060404     C                   ELSE
036400060404     C                   SETON                                        32
036500060510     C                   Z-ADD     *zeros        VABNSP
036600060404     C                   EVAL      vinmsg = %trimr(vinmsg)
036700060926     C                             + ' ' + 'VABNSP'
036800060404     C                   ENDIF
036900060926     C* RMN
037000061005     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
037100060926     C                   EXSR      CHKNUM
037200060926     C                   IF        PiInt=*on
037300060926     C                   Z-ADD     PiVal         VABRMN
037400060926     C                   ELSE
037500060926     C                   SETON                                        32
037600060926     C                   Z-ADD     1             VABRMN
037700060926     C                   EVAL      vinmsg = %trimr(vinmsg)
037800060926     C                             + ' ' + 'VABRMN'
037900060926     C                   ENDIF
038000040420     C* CAD
038100060629     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
038200010607     C                   EXSR      CHKNUM
038300010607     C                   IF        PiInt=*on
038400020204     C                   Z-ADD     PiVal         Num5_0
038500020204     C                   MOVEL(p)  Num5_0        VABCAD
038600010607     C                   ELSE
038700010607     C                   SETON                                        32
038800020204     C                   EVAL      VABCAD = *zeros
038900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039000020204     C                             + ' ' + 'VABCAD'
039100060316     C                   ENDIF
039200040420     C* NCL
039300060629     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
039400010607     C                   EXSR      CHKNUM
039500010607     C                   IF        PiInt=*on
039600010607     C                   Z-ADD     PiVal         VABNCL
039700010607     C                   ELSE
039800010607     C                   SETON                                        32
039900010607     C                   Z-ADD     *zeros        VABNCL
040000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040100010607     C                             + ' ' + 'VABNCL'
040200010607     C                   ENDIF
040300040420     C* PKB
040400060629     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
040500010607     C                   EXSR      CHKNUM
040600010607     C                   IF        PiNum=*on
040700060404     C                   Z-ADD(H)  PiVal         VABPKB
040800010607     C                   ELSE
040900010607     C                   SETON                                        32
041000010607     C                   Z-ADD     *zeros        VABPKB
041100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041200010607     C                             + ' ' + 'VABPKB'
041300010607     C                   ENDIF
041400060629     C* VLB
041500060629     C                   EVAL      PiStr=%trim(%subst(vindta:234:11))
041600060629     C                   EXSR      CHKNUM
041700060629     C                   IF        PiNum=*on
041800060629     C                   Z-ADD(H)  PiVal         VABVLB
041900060629     C                   ELSE
042000060629     C                   SETON                                        32
042100060629     C                   Z-ADD     *zeros        VABVLB
042200060629     C                   EVAL      vinmsg = %trimr(vinmsg)
042300060629     C                             + ' ' + 'VABVLB'
042400060629     C                   ENDIF
042500060516     C* CAS
042600060629     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
042700060516     C                   EXSR      CHKNUM
042800060516     C                   IF        PiNum=*on
042900060516     C                   Z-ADD     PiVal         VABCAS
043000060630     C                   IF        VABCAS > 0
043100060630     C                   EVAL      FlgCAS = '1'
043200060630     C                   EVAL      VABVCA =  'EUR'
043300060630     C                   ENDIF
043400060516     C                   ELSE
043500060516     C                   SETON                                        32
043600060629     C                   Z-ADD     *zeros        VABCAS
043700060516     C                   EVAL      vinmsg = %trimr(vinmsg)
043800060516     C                             + ' ' + 'VABCAS'
043900060516     C                   ENDIF
044000060629     C* IAS
044100060629     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
044200060629     C                   EXSR      CHKNUM
044300060629     C                   IF        PiNum=*on
044400060629     C                   Z-ADD     PiVal         VABIAS
044500060630     C                   IF        VABIAS > 0
044600060629     C                   EVAL      VABVAS =  'EUR'
044700060630     C                   ENDIF
044800060629     C                   ELSE
044900060629     C                   SETON                                        32
045000060629     C                   Z-ADD     *zeros        VABIAS
045100060629     C                   EVAL      vinmsg = %trimr(vinmsg)
045200060629     C                             + ' ' + 'VABIAS'
045300060629     C                   ENDIF
045400020322     C*
045500020322     C* Considerazioni finali su CBO/CAS
045600050908     C                   IF        FlgCAS = '1'
045700040713     C                   IF        VABCBO = '1'
045800020322     C                   EVAL      VABCBO = '4'
045900040713     C                   ENDIF
046000040713     C                   IF        VABCBO = '2'
046100040713     C                   EVAL      VABCBO = '6'
046200040713     C                   ENDIF
046300020322     C                   ENDIF
046400020204     C*
046500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046600020204     C                   EXSR      CHKIMPDIV
046700020204     C*
046800010607     C                   ENDSR
046900010607     C*----------------------------------------------------*
047000010601
047100020204
047200020204     C*----------------------------------------------------*
047300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047400020204     C*----------------------------------------------------*
047500020204     C     CHKIMPDIV     BEGSR
047600020204     C*
047700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047800020204     C                   Z-ADD     *zeros        wrkDec            9 9
047900020204     C*
048000020204     C* Come prima cosa effettuo considerazioni sulla divisa
048100020204     C                   IF        vabIAS > *zeros
048200020204     C                   IF        vabVAS <> 'EUR'
048300020204     C                   EVAL      vabVAS =  'ITL'
048400020204     C                   ENDIF
048500020204     C                   ENDIF
048600020204     C*
048700020204     C                   IF        vabCAS > *zeros
048800020204     C                   IF        vabVCA <> 'EUR'
048900020204     C                   EVAL      vabVCA =  'ITL'
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C                   IF        vabVMD > *zeros
049400020321     C                   IF        vabVAD <> 'EUR'
049500020204     C                   EVAL      vabVAD =  'ITL'
049600020204     C                   ENDIF
049700020204     C                   ENDIF
049800020204     C*
049900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050000020204     C                   Z-ADD     vabIAS        wrkDec
050100020204     C                   IF        wrkDec > *zeros
050200020204     C                   IF        vabVAS = 'ITL'
050300020204     C                   EVAL      vabIAS = *zeros
050400020204     C                   ENDIF
050500020204     C                   ENDIF
050600020204     C*
050700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050800020204     C                   Z-ADD     vabCAS        wrkDec
050900020204     C                   IF        wrkDec > *zeros
051000020204     C                   IF        vabVCA = 'ITL'
051100020204     C                   EVAL      vabCAS = *zeros
051200020204     C                   ENDIF
051300020204     C                   ENDIF
051400020204     C*
051500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051600020204     C                   Z-ADD     vabVMD        wrkDec
051700020204     C                   IF        wrkDec > *zeros
051800020204     C                   IF        vabVAD = 'ITL'
051900020204     C                   EVAL      vabVMD = *zeros
052000020204     C                   ENDIF
052100020204     C                   ENDIF
052200020204     C*
052300020204     C                   ENDSR
052400020204     C***
052500020204
052600010330
052700010330
052800010330     C*----------------------------------------------------*
052900010330     C*  CONTROLLO NUMERICITA' CAMPI
053000010330     C*----------------------------------------------------*
053100010330     C     CHKNUM        BEGSR
053200010330     C*
053300010606     C                   IF        PiDecChr = *blanks
053400060510     C                   EVAL      PiDecChr = ','
053500010606     C                   ENDIF
053600010606     C*
053700010606     C                   CALL(e)   'ISNUMERIC'
053800010330     C                   PARM                    PiStr            30
053900010606     C                   PARM                    PiDecChr          1
054000010330     C                   PARM      *ZEROS        PiVal            30 9
054100010330     C                   PARM      '0'           PiInt             1
054200010330     C                   PARM      '0'           PiNum             1
054300010330     C                   IF        %error
054400010606     C                   EVAL      PiNum=*off
054500010330     C                   ENDIF
054600010330     C*
054700010330     C                   ENDSR
054800010330     C***
054900010330
055000010601
055100010601
055200010601
055300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
055400010601     C     repfil        BEGSR
055500010601     C*
055600010601     C                   if        invfil = *zeros and
055700010601     C                             depfil > *zeros and
055800010629     C                             (vinflg = *blanks or
055900010629     C                              vinflg = *zeros)
056000010601     C
056100010601     C                   eval      invfil = depfil
056200010601     C                   endif
056300010601     C*
056400010601     C                   if        depfil <> invfil and
056500010601     C                             invfil > *zeros
056600010601     C                   eval      flgMulti = '1'
056700010601     C                   if        vinflg = *blanks
056800010601     C                   add       1             cntNonEl
056900010601     C                   endif
057000010601     C                   endif
057100010601     C*
057200010601     C                   if        vinflg = '2'
057300010601     C                   eval      flgStato = '2'
057400010601     C                   endif
057500010601     C*
057600010601     C                   ENDSR
057700010601     C***
057800010601
057900010601
058000010601
058100010330
058200010330
058300010330
058400990920      /TITLE Invio dei dati al punto operativo.
058500000613     C     invio         BEGSR
058600990920     C*
058700990920     C                   reset                   dscmz
058800010601     C                   move      invfil        cmzdst
058900990920     C                   eval      cmzfld = vlrfou
059000990920     C                   eval      cmzmbd = vlrhdl
059100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
059200000710     C                   if        prmfir = *blanks
059300050408     C                   eval      cmzfla = 'FIVAB00F'
059400050408     C                   eval      cmzmba = 'FIVAB00F'
059500000710     C                   else
059600000710     C                   eval      cmzfla = prmfir
059700000710     C                   eval      cmzmba = prmfir
059800000710     C                   endif
059900990920     C                   eval      cmznrr = *zeros
060000990920     C                   move      §ctrok        cmznrr
060100021018     C                   eval      cmzlba = vlrfl1
060200990920     C                   call(e)   'TIS711C'
060300990920     C                   parm                    dscmz
060400990921     C                   parm      *blanks       esito
060500990920     C                   if        %error
060600990920     C                             or cmzerr = '1'
060700990921     C                             or esito  = '1'
060800000710     C                   eval      wrkesito = '3'
060900990920     C                   endif
061000990920     C*
061100000613     C                   ENDSR
061200990910
061300010601
061400010601
061500010601
061600010601
061700010601      /TITLE Invio dei dati al punto operativo.
061800010601     C     opeini        BEGSR
061900010601     C*
062000010601     C* Inizializzo flag e contatori operativi
062100010601     C                   movel     '0'           flgGiro           1
062200010601     C                   movel     '0'           flgMulti          1
062300010601     C                   movel     '1'           flgStato          1
062400010615     C                   movel     '0'           flgOk             1
062500010601     C                   z-add     *zeros        cntNonEl         10 0
062600010601     C                   z-add     *zeros        depfil            3 0
062700010601     C                   z-add     *zeros        invfil            3 0
062800010601     C*
062900010601     C                   ENDSR
063000010601     C***
063100010601
063200010601
063300010601
063400010330
063500010330
063600000613     C     *inzsr        BEGSR
063700990910     C*
063800990910     C     *entry        plist
063900990920     C                   parm                    tivlrds
064000990921     C                   parm      wrkesito      esito
064100000724     C                   parm                    prmlit
064200000710     C                   parm                    prmfir
064300010330     C*
064400010330     C* CALCOLA LA DATA CORRENTE
064500010330     C                   time                    wn14             14 0
064600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064700010330     C                   z-add     wn8           g08dat
064800010330     C                   z-add     *zeros        g08inv
064900010330     C                   movel     '0'           g08err
065000010330     C                   call      'XSRDA8'
065100010330     C                   parm                    wlbda8
065200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065300000613     C*
065400000613     C                   ENDSR
065500000613     C***
065600990908
065700060629     Otitvt6p   E            riepilogo         2
065800990915     O                                              'Upload via Internet'
065900990915     O                                           +1 'Traduzione TIVIN00R -'
066000050408     O                                           +1 'FIVABWWR'
066100990915     O                                           +1 'Testate bolle'
066200990915     O          E            riepilogo   2
066300990915     O                       wrkdata
066400990915     O                       wrkora              +1
066500990915     O                       procname            +1
066600990915     O          E            riepilogo   2
066700990915     O                                              'Cliente..................:'
066800010605     O                       VABCCM        z     +1
066900990915     O          E            riepilogo   2
067000990920     O                                              'Riferimento Strategi.....:'
067100990920     O                       vlrhdl              +1
067200990915     O          E            riepilogo   2
067300990915     O                                              'Giusti...................:'
067400971022     O                       §CTROK        2   +  1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Sbagliati e corretti.....:'
067700971022     O                       §CTRMO        2   +  1
067800990915     O          E            riepilogo   2
067900990915     O                                              'Sbagliati e scartati.....:'
068000971022     O                       §CTRNO        2   +  1
068100000613
068200060629     Otitvt6ps  E            testdett          2
068300000613     O                                              'Upload via Internet'
068400000613     O                                           +1 'Traduzione TIVIN00R -'
068500050408     O                                           +1 'FIVABWWR'
068600000613     O                                           +1 'Testate bolle'
068700000616     O          E            testdett    3
068800000613     O                                           +2 'N° rec'
068900000613     O                                           +3 'Anteprima contenuto'
069000000616     O          E            rigadett    2
069100000613     O                       rrnum               +2
069200000621     O                       recko               +3
069300000616     O          E            findett     2
069400000613     O                       wrkdata
069500000613     O                       wrkora              +1
069600000613     O                       procname            +1
069700000616     O          E            findett     2
069800000613     O                                              'Cliente..................:'
069900010605     O                       VABCCM        z     +1
070000000616     O          E            findett     2
070100000613     O                                              'Riferimento Strategi.....:'
070200000613     O                       vlrhdl              +1
070300000616     O          E            findett     2
070400000613     O                                              'Giusti...................:'
070500000613     O                       §CTROK        2   +  1
070600000616     O          E            findett     2
070700000613     O                                              'Sbagliati e corretti.....:'
070800000613     O                       §CTRMO        2   +  1
070900000616     O          E            findett     2
071000000613     O                                              'Sbagliati e scartati.....:'
071100000613     O                       §CTRNO        2   +  1
071200000613** CMD - COMANDI CL
071300060629OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
071400060629OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071500060629DLTOVR FILE(TITVT6P TITVT6PS) LVL(*)
071600010606
071700000613
