000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060510     Ftitvs9p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060510     Ftitvs9ps  O    f  198        PRINTER usropn
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
009300060510     C                   if        not %open(titvs9ps)
009400060510     C                   open      titvs9ps
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
011200060510     C                   if        %open(titvs9ps)
011300000616     C                   except    findett
011400060510     C                   close     titvs9ps
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
013300060510     C                   if        not %open(titvs9p)
013400060510     C                   open      titvs9p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060510     C                   if        %open(titvs9p)
014000060510     C                   close     titvs9p
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
030000060510     C                   EVAL      VABCCM = 2750875
030100060510     C                   EVAL      VABLNP = 275
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
033900060510     C                   EVAL      VABRSD=%trim(%subst(vindta:203:30))
034000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034200020117     C* ==
034300060404     C                   ADD       1             VABNSP
034400060510     C                   EVAL      VABIND=%trim(%subst(vindta:233:25))
034500060510     C                   EVAL      VABLOD=%trim(%subst(vindta:258:30))
034600060510     C                   EVAL      VABPRD=%trim(%subst(vindta:293:2))
034700060510     C                   EVAL      VABRMA=%trim(%subst(vindta:446:10))
034800060510     C                   EVAL      VABNOT=%trim(%subst(vindta:116:20))
034900060510     C                   IF        %trim(%subst(vindta:46:2)) = '01'
035000060510     C                   EVAL      VABCBO='1'
035100060510     C                   ELSE
035200060510     C                   EVAL      VABCBO='2'
035300060510     C                   ENDIF
035400060510     C                   IF        %trim(%subst(vindta:1130:1)) = 'E'
035500060510     C                   EVAL      VABTSP='E'
035600060510     C                   ELSE
035700060510     C                   EVAL      VABTSP='C'
035800060510     C                   ENDIF
035900060510     C*
036000060510     C* Reperimento campi NUMERICI
036100060510     C                   MOVEL     DATCOR        VABAAS
036200060510     C                   MOVE      DATCOR        VABMGS
036300060510     C* NSP/RMN
036400060510     C                   EVAL      PiStr=%trim(%subst(vindta:446:10))
036500060510     C                   EXSR      CHKNUM
036600060510     C                   IF        PiInt=*on
036700060510     C                   Z-ADD     PiVal         VABNSP
036800060510     C                   Z-ADD     PiVal         VABRMN
036900060404     C                   ELSE
037000060404     C                   SETON                                        32
037100060510     C                   Z-ADD     *zeros        VABNSP
037200060510     C                   Z-ADD     1             VABRMN
037300060404     C                   EVAL      vinmsg = %trimr(vinmsg)
037400060510     C                             + ' ' + 'VABNSP VABRMN'
037500060404     C                   ENDIF
037600040420     C* CAD
037700060510     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
037800010607     C                   EXSR      CHKNUM
037900010607     C                   IF        PiInt=*on
038000020204     C                   Z-ADD     PiVal         Num5_0
038100020204     C                   MOVEL(p)  Num5_0        VABCAD
038200010607     C                   ELSE
038300010607     C                   SETON                                        32
038400020204     C                   EVAL      VABCAD = *zeros
038500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038600020204     C                             + ' ' + 'VABCAD'
038700060316     C                   ENDIF
038800040420     C* NCL
038900060510     C                   EVAL      PiStr=%trim(%subst(vindta:335:4))
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
040000060510     C                   EVAL      PiStr=%trim(%subst(vindta:371:8))
040100010607     C                   EXSR      CHKNUM
040200010607     C                   IF        PiNum=*on
040300060510     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
040400060404     C                   Z-ADD(H)  PiVal         VABPKB
040500010607     C                   ELSE
040600010607     C                   SETON                                        32
040700010607     C                   Z-ADD     *zeros        VABPKB
040800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040900010607     C                             + ' ' + 'VABPKB'
041000010607     C                   ENDIF
041100020322     C*
041200020322     C* Considerazioni finali su CBO/CAS
041300050908     C                   IF        FlgCAS = '1'
041400040713     C                   IF        VABCBO = '1'
041500020322     C                   EVAL      VABCBO = '4'
041600040713     C                   ENDIF
041700040713     C                   IF        VABCBO = '2'
041800040713     C                   EVAL      VABCBO = '6'
041900040713     C                   ENDIF
042000020322     C                   ENDIF
042100020204     C*
042200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300020204     C                   EXSR      CHKIMPDIV
042400020204     C*
042500010607     C                   ENDSR
042600010607     C*----------------------------------------------------*
042700010601
042800020204
042900020204     C*----------------------------------------------------*
043000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043100020204     C*----------------------------------------------------*
043200020204     C     CHKIMPDIV     BEGSR
043300020204     C*
043400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043500020204     C                   Z-ADD     *zeros        wrkDec            9 9
043600020204     C*
043700020204     C* Come prima cosa effettuo considerazioni sulla divisa
043800020204     C                   IF        vabIAS > *zeros
043900020204     C                   IF        vabVAS <> 'EUR'
044000020204     C                   EVAL      vabVAS =  'ITL'
044100020204     C                   ENDIF
044200020204     C                   ENDIF
044300020204     C*
044400020204     C                   IF        vabCAS > *zeros
044500020204     C                   IF        vabVCA <> 'EUR'
044600020204     C                   EVAL      vabVCA =  'ITL'
044700020204     C                   ENDIF
044800020204     C                   ENDIF
044900020204     C*
045000020204     C                   IF        vabVMD > *zeros
045100020321     C                   IF        vabVAD <> 'EUR'
045200020204     C                   EVAL      vabVAD =  'ITL'
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045700020204     C                   Z-ADD     vabIAS        wrkDec
045800020204     C                   IF        wrkDec > *zeros
045900020204     C                   IF        vabVAS = 'ITL'
046000020204     C                   EVAL      vabIAS = *zeros
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046500020204     C                   Z-ADD     vabCAS        wrkDec
046600020204     C                   IF        wrkDec > *zeros
046700020204     C                   IF        vabVCA = 'ITL'
046800020204     C                   EVAL      vabCAS = *zeros
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047300020204     C                   Z-ADD     vabVMD        wrkDec
047400020204     C                   IF        wrkDec > *zeros
047500020204     C                   IF        vabVAD = 'ITL'
047600020204     C                   EVAL      vabVMD = *zeros
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C                   ENDSR
048100020204     C***
048200020204
048300010330
048400010330
048500010330     C*----------------------------------------------------*
048600010330     C*  CONTROLLO NUMERICITA' CAMPI
048700010330     C*----------------------------------------------------*
048800010330     C     CHKNUM        BEGSR
048900010330     C*
049000010606     C                   IF        PiDecChr = *blanks
049100060510     C                   EVAL      PiDecChr = ','
049200010606     C                   ENDIF
049300010606     C*
049400010606     C                   CALL(e)   'ISNUMERIC'
049500010330     C                   PARM                    PiStr            30
049600010606     C                   PARM                    PiDecChr          1
049700010330     C                   PARM      *ZEROS        PiVal            30 9
049800010330     C                   PARM      '0'           PiInt             1
049900010330     C                   PARM      '0'           PiNum             1
050000010330     C                   IF        %error
050100010606     C                   EVAL      PiNum=*off
050200010330     C                   ENDIF
050300010330     C*
050400010330     C                   ENDSR
050500010330     C***
050600010330
050700010601
050800010601
050900010601
051000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051100010601     C     repfil        BEGSR
051200010601     C*
051300010601     C                   if        invfil = *zeros and
051400010601     C                             depfil > *zeros and
051500010629     C                             (vinflg = *blanks or
051600010629     C                              vinflg = *zeros)
051700010601     C
051800010601     C                   eval      invfil = depfil
051900010601     C                   endif
052000010601     C*
052100010601     C                   if        depfil <> invfil and
052200010601     C                             invfil > *zeros
052300010601     C                   eval      flgMulti = '1'
052400010601     C                   if        vinflg = *blanks
052500010601     C                   add       1             cntNonEl
052600010601     C                   endif
052700010601     C                   endif
052800010601     C*
052900010601     C                   if        vinflg = '2'
053000010601     C                   eval      flgStato = '2'
053100010601     C                   endif
053200010601     C*
053300010601     C                   ENDSR
053400010601     C***
053500010601
053600010601
053700010601
053800010330
053900010330
054000010330
054100990920      /TITLE Invio dei dati al punto operativo.
054200000613     C     invio         BEGSR
054300990920     C*
054400990920     C                   reset                   dscmz
054500010601     C                   move      invfil        cmzdst
054600990920     C                   eval      cmzfld = vlrfou
054700990920     C                   eval      cmzmbd = vlrhdl
054800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054900000710     C                   if        prmfir = *blanks
055000050408     C                   eval      cmzfla = 'FIVAB00F'
055100050408     C                   eval      cmzmba = 'FIVAB00F'
055200000710     C                   else
055300000710     C                   eval      cmzfla = prmfir
055400000710     C                   eval      cmzmba = prmfir
055500000710     C                   endif
055600990920     C                   eval      cmznrr = *zeros
055700990920     C                   move      §ctrok        cmznrr
055800021018     C                   eval      cmzlba = vlrfl1
055900990920     C                   call(e)   'TIS711C'
056000990920     C                   parm                    dscmz
056100990921     C                   parm      *blanks       esito
056200990920     C                   if        %error
056300990920     C                             or cmzerr = '1'
056400990921     C                             or esito  = '1'
056500000710     C                   eval      wrkesito = '3'
056600990920     C                   endif
056700990920     C*
056800000613     C                   ENDSR
056900990910
057000010601
057100010601
057200010601
057300010601
057400010601      /TITLE Invio dei dati al punto operativo.
057500010601     C     opeini        BEGSR
057600010601     C*
057700010601     C* Inizializzo flag e contatori operativi
057800010601     C                   movel     '0'           flgGiro           1
057900010601     C                   movel     '0'           flgMulti          1
058000010601     C                   movel     '1'           flgStato          1
058100010615     C                   movel     '0'           flgOk             1
058200010601     C                   z-add     *zeros        cntNonEl         10 0
058300010601     C                   z-add     *zeros        depfil            3 0
058400010601     C                   z-add     *zeros        invfil            3 0
058500010601     C*
058600010601     C                   ENDSR
058700010601     C***
058800010601
058900010601
059000010601
059100010330
059200010330
059300000613     C     *inzsr        BEGSR
059400990910     C*
059500990910     C     *entry        plist
059600990920     C                   parm                    tivlrds
059700990921     C                   parm      wrkesito      esito
059800000724     C                   parm                    prmlit
059900000710     C                   parm                    prmfir
060000010330     C*
060100010330     C* CALCOLA LA DATA CORRENTE
060200010330     C                   time                    wn14             14 0
060300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060400010330     C                   z-add     wn8           g08dat
060500010330     C                   z-add     *zeros        g08inv
060600010330     C                   movel     '0'           g08err
060700010330     C                   call      'XSRDA8'
060800010330     C                   parm                    wlbda8
060900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061000000613     C*
061100000613     C                   ENDSR
061200000613     C***
061300990908
061400060510     Otitvs9p   E            riepilogo         2
061500990915     O                                              'Upload via Internet'
061600990915     O                                           +1 'Traduzione TIVIN00R -'
061700050408     O                                           +1 'FIVABWWR'
061800990915     O                                           +1 'Testate bolle'
061900990915     O          E            riepilogo   2
062000990915     O                       wrkdata
062100990915     O                       wrkora              +1
062200990915     O                       procname            +1
062300990915     O          E            riepilogo   2
062400990915     O                                              'Cliente..................:'
062500010605     O                       VABCCM        z     +1
062600990915     O          E            riepilogo   2
062700990920     O                                              'Riferimento Strategi.....:'
062800990920     O                       vlrhdl              +1
062900990915     O          E            riepilogo   2
063000990915     O                                              'Giusti...................:'
063100971022     O                       §CTROK        2   +  1
063200990915     O          E            riepilogo   2
063300990915     O                                              'Sbagliati e corretti.....:'
063400971022     O                       §CTRMO        2   +  1
063500990915     O          E            riepilogo   2
063600990915     O                                              'Sbagliati e scartati.....:'
063700971022     O                       §CTRNO        2   +  1
063800000613
063900060510     Otitvs9ps  E            testdett          2
064000000613     O                                              'Upload via Internet'
064100000613     O                                           +1 'Traduzione TIVIN00R -'
064200050408     O                                           +1 'FIVABWWR'
064300000613     O                                           +1 'Testate bolle'
064400000616     O          E            testdett    3
064500000613     O                                           +2 'N° rec'
064600000613     O                                           +3 'Anteprima contenuto'
064700000616     O          E            rigadett    2
064800000613     O                       rrnum               +2
064900000621     O                       recko               +3
065000000616     O          E            findett     2
065100000613     O                       wrkdata
065200000613     O                       wrkora              +1
065300000613     O                       procname            +1
065400000616     O          E            findett     2
065500000613     O                                              'Cliente..................:'
065600010605     O                       VABCCM        z     +1
065700000616     O          E            findett     2
065800000613     O                                              'Riferimento Strategi.....:'
065900000613     O                       vlrhdl              +1
066000000616     O          E            findett     2
066100000613     O                                              'Giusti...................:'
066200000613     O                       §CTROK        2   +  1
066300000616     O          E            findett     2
066400000613     O                                              'Sbagliati e corretti.....:'
066500000613     O                       §CTRMO        2   +  1
066600000616     O          E            findett     2
066700000613     O                                              'Sbagliati e scartati.....:'
066800000613     O                       §CTRNO        2   +  1
066900000613** CMD - COMANDI CL
067000060510OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067100060510OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067200060510DLTOVR FILE(TITVS9P TITVS9PS) LVL(*)
067300010606
067400000613
