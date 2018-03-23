000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060516     Ftitvt1p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060516     Ftitvt1ps  O    f  198        PRINTER usropn
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
009300060516     C                   if        not %open(titvt1ps)
009400060516     C                   open      titvt1ps
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
011200060516     C                   if        %open(titvt1ps)
011300000616     C                   except    findett
011400060516     C                   close     titvt1ps
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
013300060516     C                   if        not %open(titvt1p)
013400060516     C                   open      titvt1p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060516     C                   if        %open(titvt1p)
014000060516     C                   close     titvt1p
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
030000060516     C                   EVAL      VABCCM = 1310268
030100060516     C                   EVAL      VABLNP = 131
030200060105     C                   EVAL      VABCTR = 000
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
033900060516     C                   EVAL      VABRSD=%trim(%subst(vindta:35:35))
034000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034200020117     C* ==
034300060516     C                   EVAL      VABRD2=%trim(%subst(vindta:70:35))
034400060516     C                   EVAL      VABIND=%trim(%subst(vindta:105:35))
034500060516     C                   EVAL      VABLOD=%trim(%subst(vindta:149:35))
034600060516     C                   EVAL      VABPRD=%trim(%subst(vindta:184:2))
034700060516     C                   EVAL      VABRMA=%trim(%subst(vindta:24:6))
034800060516     C                   EVAL      VABNOT=%trim(%subst(vindta:325:34))
034900060516     C                   EVAL      VABNT2=%trim(%subst(vindta:358:34))
035000060516     C                   EVAL      VABCBO=%trim(%subst(vindta:30:1))
035100060516     C                   EVAL      VABVCA=%trim(%subst(vindta:275:3))
035200060516     C                   EVAL      VABTIC=%trim(%subst(vindta:273:2))
035300060516     C                   EVAL      VABTSP=%trim(%subst(vindta:197:1))
035400060510     C*
035500060510     C* Reperimento campi NUMERICI
035600060510     C                   MOVEL     DATCOR        VABAAS
035700060510     C                   MOVE      DATCOR        VABMGS
035800060510     C* NSP/RMN
035900060516     C                   EVAL      PiStr=%trim(%subst(vindta:24:6))
036000060510     C                   EXSR      CHKNUM
036100060510     C                   IF        PiInt=*on
036200060510     C                   Z-ADD     PiVal         VABNSP
036300060510     C                   Z-ADD     PiVal         VABRMN
036400060404     C                   ELSE
036500060404     C                   SETON                                        32
036600060510     C                   Z-ADD     *zeros        VABNSP
036700060510     C                   Z-ADD     1             VABRMN
036800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
036900060510     C                             + ' ' + 'VABNSP VABRMN'
037000060404     C                   ENDIF
037100040420     C* CAD
037200060516     C                   EVAL      PiStr=%trim(%subst(vindta:140:5))
037300010607     C                   EXSR      CHKNUM
037400010607     C                   IF        PiInt=*on
037500020204     C                   Z-ADD     PiVal         Num5_0
037600020204     C                   MOVEL(p)  Num5_0        VABCAD
037700010607     C                   ELSE
037800010607     C                   SETON                                        32
037900020204     C                   EVAL      VABCAD = *zeros
038000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038100020204     C                             + ' ' + 'VABCAD'
038200060316     C                   ENDIF
038300040420     C* NCL
038400060516     C                   EVAL      PiStr=%trim(%subst(vindta:230:5))
038500010607     C                   EXSR      CHKNUM
038600010607     C                   IF        PiInt=*on
038700010607     C                   Z-ADD     PiVal         VABNCL
038800010607     C                   ELSE
038900010607     C                   SETON                                        32
039000010607     C                   Z-ADD     *zeros        VABNCL
039100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039200010607     C                             + ' ' + 'VABNCL'
039300010607     C                   ENDIF
039400060516     C* CCM
039500060516     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
039600060516     C                   EXSR      CHKNUM
039700060516     C                   IF        PiInt=*on
039800060516     C                   Z-ADD     PiVal         VABCCM
039900060516     C                   ELSE
040000060516     C                   SETON                                        32
040100060516     C                   EVAL      vinmsg = %trimr(vinmsg)
040200060516     C                             + ' ' + 'VABCCM'
040300060516     C                   ENDIF
040400060516     C* CTR
040500060516     C                   EVAL      PiStr=%trim(%subst(vindta:194:3))
040600060516     C                   EXSR      CHKNUM
040700060516     C                   IF        PiInt=*on
040800060516     C                   Z-ADD     PiVal         VABCTR
040900060516     C                   ELSE
041000060516     C                   SETON                                        32
041100060516     C                   EVAL      vinmsg = %trimr(vinmsg)
041200060516     C                             + ' ' + 'VABCTR'
041300060516     C                   ENDIF
041400040420     C* PKB
041500060516     C                   EVAL      PiStr=%trim(%subst(vindta:235:7))
041600010607     C                   EXSR      CHKNUM
041700010607     C                   IF        PiNum=*on
041800060404     C                   Z-ADD(H)  PiVal         VABPKB
041900010607     C                   ELSE
042000010607     C                   SETON                                        32
042100010607     C                   Z-ADD     *zeros        VABPKB
042200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042300010607     C                             + ' ' + 'VABPKB'
042400010607     C                   ENDIF
042500060516     C* CAS
042600060516     C                   IF        %trim(%subst(vindta:260:13)) <> '0,000'
042700060516     C                   EVAL      FlgCAS = '1'
042800060516     C                   EVAL      PiStr=%trim(%subst(vindta:260:13))
042900060516     C                   EXSR      CHKNUM
043000060516     C                   IF        PiNum=*on
043100060516     C                   Z-ADD     PiVal         VABCAS
043200060516     C                   ELSE
043300060516     C                   SETON                                        32
043400060629     C                   Z-ADD     *zeros        VABCAS
043500060516     C                   EVAL      vinmsg = %trimr(vinmsg)
043600060516     C                             + ' ' + 'VABCAS'
043700060516     C                   ENDIF
043800060516     C                   ENDIF
043900020322     C*
044000020322     C* Considerazioni finali su CBO/CAS
044100050908     C                   IF        FlgCAS = '1'
044200040713     C                   IF        VABCBO = '1'
044300020322     C                   EVAL      VABCBO = '4'
044400040713     C                   ENDIF
044500040713     C                   IF        VABCBO = '2'
044600040713     C                   EVAL      VABCBO = '6'
044700040713     C                   ENDIF
044800020322     C                   ENDIF
044900020204     C*
045000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045100020204     C                   EXSR      CHKIMPDIV
045200020204     C*
045300010607     C                   ENDSR
045400010607     C*----------------------------------------------------*
045500010601
045600020204
045700020204     C*----------------------------------------------------*
045800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045900020204     C*----------------------------------------------------*
046000020204     C     CHKIMPDIV     BEGSR
046100020204     C*
046200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046300020204     C                   Z-ADD     *zeros        wrkDec            9 9
046400020204     C*
046500020204     C* Come prima cosa effettuo considerazioni sulla divisa
046600020204     C                   IF        vabIAS > *zeros
046700020204     C                   IF        vabVAS <> 'EUR'
046800020204     C                   EVAL      vabVAS =  'ITL'
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C                   IF        vabCAS > *zeros
047300020204     C                   IF        vabVCA <> 'EUR'
047400020204     C                   EVAL      vabVCA =  'ITL'
047500020204     C                   ENDIF
047600020204     C                   ENDIF
047700020204     C*
047800020204     C                   IF        vabVMD > *zeros
047900020321     C                   IF        vabVAD <> 'EUR'
048000020204     C                   EVAL      vabVAD =  'ITL'
048100020204     C                   ENDIF
048200020204     C                   ENDIF
048300020204     C*
048400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048500020204     C                   Z-ADD     vabIAS        wrkDec
048600020204     C                   IF        wrkDec > *zeros
048700020204     C                   IF        vabVAS = 'ITL'
048800020204     C                   EVAL      vabIAS = *zeros
048900020204     C                   ENDIF
049000020204     C                   ENDIF
049100020204     C*
049200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049300020204     C                   Z-ADD     vabCAS        wrkDec
049400020204     C                   IF        wrkDec > *zeros
049500020204     C                   IF        vabVCA = 'ITL'
049600020204     C                   EVAL      vabCAS = *zeros
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050100020204     C                   Z-ADD     vabVMD        wrkDec
050200020204     C                   IF        wrkDec > *zeros
050300020204     C                   IF        vabVAD = 'ITL'
050400020204     C                   EVAL      vabVMD = *zeros
050500020204     C                   ENDIF
050600020204     C                   ENDIF
050700020204     C*
050800020204     C                   ENDSR
050900020204     C***
051000020204
051100010330
051200010330
051300010330     C*----------------------------------------------------*
051400010330     C*  CONTROLLO NUMERICITA' CAMPI
051500010330     C*----------------------------------------------------*
051600010330     C     CHKNUM        BEGSR
051700010330     C*
051800010606     C                   IF        PiDecChr = *blanks
051900060510     C                   EVAL      PiDecChr = ','
052000010606     C                   ENDIF
052100010606     C*
052200010606     C                   CALL(e)   'ISNUMERIC'
052300010330     C                   PARM                    PiStr            30
052400010606     C                   PARM                    PiDecChr          1
052500010330     C                   PARM      *ZEROS        PiVal            30 9
052600010330     C                   PARM      '0'           PiInt             1
052700010330     C                   PARM      '0'           PiNum             1
052800010330     C                   IF        %error
052900010606     C                   EVAL      PiNum=*off
053000010330     C                   ENDIF
053100010330     C*
053200010330     C                   ENDSR
053300010330     C***
053400010330
053500010601
053600010601
053700010601
053800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053900010601     C     repfil        BEGSR
054000010601     C*
054100010601     C                   if        invfil = *zeros and
054200010601     C                             depfil > *zeros and
054300010629     C                             (vinflg = *blanks or
054400010629     C                              vinflg = *zeros)
054500010601     C
054600010601     C                   eval      invfil = depfil
054700010601     C                   endif
054800010601     C*
054900010601     C                   if        depfil <> invfil and
055000010601     C                             invfil > *zeros
055100010601     C                   eval      flgMulti = '1'
055200010601     C                   if        vinflg = *blanks
055300010601     C                   add       1             cntNonEl
055400010601     C                   endif
055500010601     C                   endif
055600010601     C*
055700010601     C                   if        vinflg = '2'
055800010601     C                   eval      flgStato = '2'
055900010601     C                   endif
056000010601     C*
056100010601     C                   ENDSR
056200010601     C***
056300010601
056400010601
056500010601
056600010330
056700010330
056800010330
056900990920      /TITLE Invio dei dati al punto operativo.
057000000613     C     invio         BEGSR
057100990920     C*
057200990920     C                   reset                   dscmz
057300010601     C                   move      invfil        cmzdst
057400990920     C                   eval      cmzfld = vlrfou
057500990920     C                   eval      cmzmbd = vlrhdl
057600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057700000710     C                   if        prmfir = *blanks
057800050408     C                   eval      cmzfla = 'FIVAB00F'
057900050408     C                   eval      cmzmba = 'FIVAB00F'
058000000710     C                   else
058100000710     C                   eval      cmzfla = prmfir
058200000710     C                   eval      cmzmba = prmfir
058300000710     C                   endif
058400990920     C                   eval      cmznrr = *zeros
058500990920     C                   move      §ctrok        cmznrr
058600021018     C                   eval      cmzlba = vlrfl1
058700990920     C                   call(e)   'TIS711C'
058800990920     C                   parm                    dscmz
058900990921     C                   parm      *blanks       esito
059000990920     C                   if        %error
059100990920     C                             or cmzerr = '1'
059200990921     C                             or esito  = '1'
059300000710     C                   eval      wrkesito = '3'
059400990920     C                   endif
059500990920     C*
059600000613     C                   ENDSR
059700990910
059800010601
059900010601
060000010601
060100010601
060200010601      /TITLE Invio dei dati al punto operativo.
060300010601     C     opeini        BEGSR
060400010601     C*
060500010601     C* Inizializzo flag e contatori operativi
060600010601     C                   movel     '0'           flgGiro           1
060700010601     C                   movel     '0'           flgMulti          1
060800010601     C                   movel     '1'           flgStato          1
060900010615     C                   movel     '0'           flgOk             1
061000010601     C                   z-add     *zeros        cntNonEl         10 0
061100010601     C                   z-add     *zeros        depfil            3 0
061200010601     C                   z-add     *zeros        invfil            3 0
061300010601     C*
061400010601     C                   ENDSR
061500010601     C***
061600010601
061700010601
061800010601
061900010330
062000010330
062100000613     C     *inzsr        BEGSR
062200990910     C*
062300990910     C     *entry        plist
062400990920     C                   parm                    tivlrds
062500990921     C                   parm      wrkesito      esito
062600000724     C                   parm                    prmlit
062700000710     C                   parm                    prmfir
062800010330     C*
062900010330     C* CALCOLA LA DATA CORRENTE
063000010330     C                   time                    wn14             14 0
063100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063200010330     C                   z-add     wn8           g08dat
063300010330     C                   z-add     *zeros        g08inv
063400010330     C                   movel     '0'           g08err
063500010330     C                   call      'XSRDA8'
063600010330     C                   parm                    wlbda8
063700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063800000613     C*
063900000613     C                   ENDSR
064000000613     C***
064100990908
064200060516     Otitvt1p   E            riepilogo         2
064300990915     O                                              'Upload via Internet'
064400990915     O                                           +1 'Traduzione TIVIN00R -'
064500050408     O                                           +1 'FIVABWWR'
064600990915     O                                           +1 'Testate bolle'
064700990915     O          E            riepilogo   2
064800990915     O                       wrkdata
064900990915     O                       wrkora              +1
065000990915     O                       procname            +1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Cliente..................:'
065300010605     O                       VABCCM        z     +1
065400990915     O          E            riepilogo   2
065500990920     O                                              'Riferimento Strategi.....:'
065600990920     O                       vlrhdl              +1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Giusti...................:'
065900971022     O                       §CTROK        2   +  1
066000990915     O          E            riepilogo   2
066100990915     O                                              'Sbagliati e corretti.....:'
066200971022     O                       §CTRMO        2   +  1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Sbagliati e scartati.....:'
066500971022     O                       §CTRNO        2   +  1
066600000613
066700060516     Otitvt1ps  E            testdett          2
066800000613     O                                              'Upload via Internet'
066900000613     O                                           +1 'Traduzione TIVIN00R -'
067000050408     O                                           +1 'FIVABWWR'
067100000613     O                                           +1 'Testate bolle'
067200000616     O          E            testdett    3
067300000613     O                                           +2 'N° rec'
067400000613     O                                           +3 'Anteprima contenuto'
067500000616     O          E            rigadett    2
067600000613     O                       rrnum               +2
067700000621     O                       recko               +3
067800000616     O          E            findett     2
067900000613     O                       wrkdata
068000000613     O                       wrkora              +1
068100000613     O                       procname            +1
068200000616     O          E            findett     2
068300000613     O                                              'Cliente..................:'
068400010605     O                       VABCCM        z     +1
068500000616     O          E            findett     2
068600000613     O                                              'Riferimento Strategi.....:'
068700000613     O                       vlrhdl              +1
068800000616     O          E            findett     2
068900000613     O                                              'Giusti...................:'
069000000613     O                       §CTROK        2   +  1
069100000616     O          E            findett     2
069200000613     O                                              'Sbagliati e corretti.....:'
069300000613     O                       §CTRMO        2   +  1
069400000616     O          E            findett     2
069500000613     O                                              'Sbagliati e scartati.....:'
069600000613     O                       §CTRNO        2   +  1
069700000613** CMD - COMANDI CL
069800060516OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069900060516OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVT1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070000060516DLTOVR FILE(TITVT1P TITVT1PS) LVL(*)
070100010606
070200000613
