000100050408      /TITLE Upload via Internet: traduzione in FIVABWWR. (x REVIVRE Cod. 0610403)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700050408     Ftitvl8p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050408     Ftitvl8ps  O    f  198        PRINTER usropn
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
002300050330     D tisi95ds      e ds
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
009400050408     C                   if        not %open(titvl8ps)
009500050408     C                   open      titvl8ps
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
011300050408     C                   if        %open(titvl8ps)
011400000616     C                   except    findett
011500050408     C                   close     titvl8ps
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
013400050408     C                   if        not %open(titvl8p)
013500050408     C                   open      titvl8p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000050408     C                   if        %open(titvl8p)
014100050408     C                   close     titvl8p
014200990914     C                   endif
014300000613     C*
014400000613     C                   ENDSR
014500000613     C***
014600000613
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300050408     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700050408     C                   if        not %open(fivabwwr)
015800050408     C                   open      fivabwwr
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
020000010601     C*
020100010601     C                   exsr      PREELA
020200010601     C*
020300010604     C* Ebbene...
020400010604     C*
020500010604     C  N31              ADD       1             §CTROK            7 0
020600010604     C   32              ADD       1             §CTRMO            7 0
020700010604     C   31              ADD       1             §CTRNO            7 0
020800020205     C*
020900050408     C* Scarico il buffer del FIVAB
021000050408     C  N31              WRITE     FIVAB000
021100010604     C*
021200020722     C                   endif
021300020722     C*
021400010604     C                   if        *in31 = *off and
021500010604     C                             *in32 = *off
021600010604     C                   eval      vinflg = '1'
021700010604     C                   else
021800010604     C                   eval      recko = vindta
021900010604     C                   except    rigadett
022000010604     C                   eval      vinflg = '2'
022100010604     C                   endif
022200010604     C                   endif
022300010604     C*
022400010330     C                   else
022500010330     C                   eval      vinflg = '1'
022600010330     C                   endif
022700010601     C*
022800010601     C  N70              update    tivin000
022900010330     C*
023000010330     C  N70              ENDdo
023100010601     C*
023200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023300010601     C                   if        cntNonEl = *zeros or
023400010601     C                             flgMulti = '0'
023500010330     C* Se non ci sono record con errori ...
023600010601     C                   if        §ctrno = 0 and
023700010604     C                             §ctrmo = 0 and
023800010601     C                             flgStato <> '2'
023900010330     C* ... restituisco esito OK.
024000010330     C                   eval      wrkesito = '0'
024100010330     C                   else
024200010330     C                   if        §ctrok > 0
024300010330     C                   eval      wrkesito = '1'
024400010330     C                   else
024500010615     C                   if        flgOk = '0'
024600010615     C                   eval      wrkesito = '2'
024700010615     C                   else
024800010615     C                   eval      wrkesito = '6'
024900010615     C                   endif
025000010330     C                   endif
025100010330     C                   endif
025200010601     C                   else
025300010601     C                   eval      wrkesito = '9'
025400010601     C                   endif
025500010330     C*
025600010330     C                   if        %open(tivin00r)
025700010330     C                   close     tivin00r
025800010330     C                   endif
025900050408     C                   if        %open(fivabwwr)
026000050408     C                   close     fivabwwr
026100010330     C                   endif
026200010601     C*
026300010601     C                   if        vlrpoi <> 999
026400010601     C                   eval      invfil = vlrpoi
026500010601     C                   endif
026600010330     C*
026700010330     C                   if        §ctrok > 0
026800010601     C                             and invfil > *zeros
026900010330     C                   exsr      invio
027000010330     C                   endif
027100010601     C*
027200010618     C                   if        flgGiro = '1'
027300010601     C                   exsr      endela
027400010618     C                   endif
027500010330     C*
027600010330     C                   ENDSR
027700010330     C***
027800010330
027900010601
028000010601
028100010601
028200010601
028300010330     C*----------------------------------------------------*
028400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028500010330     C*----------------------------------------------------*
028600010330     C     INZVAR        BEGSR
028700010330     C*
028800020204     C                   Z-ADD     *zeros        Num5_0            5 0
028900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029000020322     C                   MOVEL     '0'           FlgCAS            1
029100010330     C*
029200010330     C                   ENDSR
029300010330     C*----------------------------------------------------*
029400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029500010330     C*----------------------------------------------------*
029600010330     C     DEFCAM        BEGSR
029700010330     C*
029800020204     C* Inizializzo il buffer del record da scrivere
029900050408     C                   CLEAR                   FIVAB000
030000020204     C* Imposto i valori di default...
030100050408     C                   EVAL      VABCCM = 0610403
030200050408     C                   EVAL      VABLNP = 061
030300040713     C                   EVAL      VABCTR = 000
030400050408     C                   EVAL      VABGC1 = '1'
030500050408     C                   EVAL      VABCBO = '1'
030600020204     C* ... e poi verifico se sono stati passati come parametri
030700020204     C                   IF        vlrppt > *blanks
030800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031000020204     C                   EXSR      CHKNUM
031100020204     C                   IF        PiInt=*on
031200020204     C                   Z-ADD     PiVal         VABCCM
031300020204     C                   ENDIF
031400040301     C                   ENDIF
031500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031700020204     C                   EXSR      CHKNUM
031800020204     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         VABLNP
032000020204     C                   ENDIF
032100040301     C                   ENDIF
032200040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032400020204     C                   EXSR      CHKNUM
032500020204     C                   IF        PiInt=*on
032600020204     C                   Z-ADD     PiVal         VABCTR
032700040301     C                   ENDIF
032800020204     C                   ENDIF
032900020204     C                   ENDIF
033000020204     C*
033100010330     C                   ENDSR
033200010607     C*----------------------------------------------------*
033300050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033400010607     C*----------------------------------------------------*
033500010607     C     IMPVAB        BEGSR
033600010607     C*
033700010607     C                   SETOFF                                       3132
033800010607     C*
033900010607     C* Reperimento campi ALFA
034000010607     C*
034100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034200050408     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
034300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034500020117     C* ==
034600050408     C                   EVAL      VABIND=%trim(%subst(vindta:65:35))
034700050408     C                   EVAL      VABLOD=%trim(%subst(vindta:121:35))
034800050408     C                   EVAL      VABPRD=%trim(%subst(vindta:169:2))
034900050408     C                   EVAL      VABRMA=%trim(%subst(vindta:1:8))
035000010607     C*
035100010607     C* Reperimento campi NUMERICI
035200020722     C                   MOVEL     DATCOR        VABAAS
035300020722     C                   MOVE      DATCOR        VABMGS
035400050408     C* NSP/RMN
035500050408     C                   EVAL      PiStr=%trim(%subst(vindta:177:8))
035600050408     C                   EXSR      CHKNUM
035700050408     C                   IF        PiInt=*on
035800050408     C                   Z-ADD     PiVal         VABNSP
035900050408     C                   Z-ADD     PiVal         VABRMN
036000050408     C                   ELSE
036100050408     C                   SETON                                        31
036200050408     C                   Z-ADD     *zeros        VABNSP
036300050408     C                   Z-ADD     *zeros        VABRMN
036400050408     C                   EVAL      vinmsg = %trimr(vinmsg)
036500050408     C                             + ' ' + 'VABNSP VABRMN'
036600050408     C                   ENDIF
036700040420     C* CAD
036800050408     C                   EVAL      PiStr=%trim(%subst(vindta:113:5))
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
037900040420     C* NCL
038000050408     C                   EVAL      PiStr=%trim(%subst(vindta:217:4))
038100010607     C                   EXSR      CHKNUM
038200010607     C                   IF        PiInt=*on
038300010607     C                   Z-ADD     PiVal         VABNCL
038400010607     C                   ELSE
038500010607     C                   SETON                                        32
038600010607     C                   Z-ADD     *zeros        VABNCL
038700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038800010607     C                             + ' ' + 'VABNCL'
038900010607     C                   ENDIF
039000040420     C* PKB
039100050408     C                   EVAL      PiStr=%trim(%subst(vindta:225:10))
039200010607     C                   EXSR      CHKNUM
039300010607     C                   IF        PiNum=*on
039400050330     C                   Z-ADD(H)  PiVal         VABPKB
039500010607     C                   ELSE
039600010607     C                   SETON                                        32
039700010607     C                   Z-ADD     *zeros        VABPKB
039800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039900010607     C                             + ' ' + 'VABPKB'
040000010607     C                   ENDIF
040100020322     C*
040200020322     C* Considerazioni finali su CBO/CAS
040300050408     C                   IF        %subst(vindta:241:2) = 'SI'
040400040713     C                   IF        VABCBO = '1'
040500020322     C                   EVAL      VABCBO = '4'
040600040713     C                   ENDIF
040700040713     C                   IF        VABCBO = '2'
040800040713     C                   EVAL      VABCBO = '6'
040900040713     C                   ENDIF
041000020322     C                   ENDIF
041100020204     C*
041200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041300020204     C                   EXSR      CHKIMPDIV
041400020204     C*
041500010607     C                   ENDSR
041600010607     C*----------------------------------------------------*
041700010601
041800020204
041900020204     C*----------------------------------------------------*
042000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042100020204     C*----------------------------------------------------*
042200020204     C     CHKIMPDIV     BEGSR
042300020204     C*
042400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042500020204     C                   Z-ADD     *zeros        wrkDec            9 9
042600020204     C*
042700020204     C* Come prima cosa effettuo considerazioni sulla divisa
042800020204     C                   IF        vabIAS > *zeros
042900020204     C                   IF        vabVAS <> 'EUR'
043000020204     C                   EVAL      vabVAS =  'ITL'
043100020204     C                   ENDIF
043200020204     C                   ENDIF
043300020204     C*
043400020204     C                   IF        vabCAS > *zeros
043500020204     C                   IF        vabVCA <> 'EUR'
043600020204     C                   EVAL      vabVCA =  'ITL'
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C                   IF        vabVMD > *zeros
044100020321     C                   IF        vabVAD <> 'EUR'
044200020204     C                   EVAL      vabVAD =  'ITL'
044300020204     C                   ENDIF
044400020204     C                   ENDIF
044500020204     C*
044600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044700020204     C                   Z-ADD     vabIAS        wrkDec
044800020204     C                   IF        wrkDec > *zeros
044900020204     C                   IF        vabVAS = 'ITL'
045000020204     C                   EVAL      vabIAS = *zeros
045100020204     C                   ENDIF
045200020204     C                   ENDIF
045300020204     C*
045400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
045500020204     C                   Z-ADD     vabCAS        wrkDec
045600020204     C                   IF        wrkDec > *zeros
045700020204     C                   IF        vabVCA = 'ITL'
045800020204     C                   EVAL      vabCAS = *zeros
045900020204     C                   ENDIF
046000020204     C                   ENDIF
046100020204     C*
046200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046300020204     C                   Z-ADD     vabVMD        wrkDec
046400020204     C                   IF        wrkDec > *zeros
046500020204     C                   IF        vabVAD = 'ITL'
046600020204     C                   EVAL      vabVMD = *zeros
046700020204     C                   ENDIF
046800020204     C                   ENDIF
046900020204     C*
047000020204     C                   ENDSR
047100020204     C***
047200020204
047300010330
047400010330
047500010330     C*----------------------------------------------------*
047600010330     C*  CONTROLLO NUMERICITA' CAMPI
047700010330     C*----------------------------------------------------*
047800010330     C     CHKNUM        BEGSR
047900010330     C*
048000010606     C                   IF        PiDecChr = *blanks
048100010606     C                   EVAL      PiDecChr = ','
048200010606     C                   ENDIF
048300010606     C*
048400010606     C                   CALL(e)   'ISNUMERIC'
048500010330     C                   PARM                    PiStr            30
048600010606     C                   PARM                    PiDecChr          1
048700010330     C                   PARM      *ZEROS        PiVal            30 9
048800010330     C                   PARM      '0'           PiInt             1
048900010330     C                   PARM      '0'           PiNum             1
049000010330     C                   IF        %error
049100010606     C                   EVAL      PiNum=*off
049200010330     C                   ENDIF
049300010330     C*
049400010330     C                   ENDSR
049500010330     C***
049600010330
049700010601
049800010601
049900010601
050000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050100010601     C     repfil        BEGSR
050200010601     C*
050300010601     C                   if        invfil = *zeros and
050400010601     C                             depfil > *zeros and
050500010629     C                             (vinflg = *blanks or
050600010629     C                              vinflg = *zeros)
050700010601     C
050800010601     C                   eval      invfil = depfil
050900010601     C                   endif
051000010601     C*
051100010601     C                   if        depfil <> invfil and
051200010601     C                             invfil > *zeros
051300010601     C                   eval      flgMulti = '1'
051400010601     C                   if        vinflg = *blanks
051500010601     C                   add       1             cntNonEl
051600010601     C                   endif
051700010601     C                   endif
051800010601     C*
051900010601     C                   if        vinflg = '2'
052000010601     C                   eval      flgStato = '2'
052100010601     C                   endif
052200010601     C*
052300010601     C                   ENDSR
052400010601     C***
052500010601
052600010601
052700010601
052800010330
052900010330
053000010330
053100990920      /TITLE Invio dei dati al punto operativo.
053200000613     C     invio         BEGSR
053300990920     C*
053400990920     C                   reset                   dscmz
053500010601     C                   move      invfil        cmzdst
053600990920     C                   eval      cmzfld = vlrfou
053700990920     C                   eval      cmzmbd = vlrhdl
053800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
053900000710     C                   if        prmfir = *blanks
054000050408     C                   eval      cmzfla = 'FIVAB00F'
054100050408     C                   eval      cmzmba = 'FIVAB00F'
054200000710     C                   else
054300000710     C                   eval      cmzfla = prmfir
054400000710     C                   eval      cmzmba = prmfir
054500000710     C                   endif
054600990920     C                   eval      cmznrr = *zeros
054700990920     C                   move      §ctrok        cmznrr
054800021018     C                   eval      cmzlba = vlrfl1
054900990920     C                   call(e)   'TIS711C'
055000990920     C                   parm                    dscmz
055100990921     C                   parm      *blanks       esito
055200990920     C                   if        %error
055300990920     C                             or cmzerr = '1'
055400990921     C                             or esito  = '1'
055500000710     C                   eval      wrkesito = '3'
055600990920     C                   endif
055700990920     C*
055800000613     C                   ENDSR
055900990910
056000010601
056100010601
056200010601
056300010601
056400010601      /TITLE Invio dei dati al punto operativo.
056500010601     C     opeini        BEGSR
056600010601     C*
056700010601     C* Inizializzo flag e contatori operativi
056800010601     C                   movel     '0'           flgGiro           1
056900010601     C                   movel     '0'           flgMulti          1
057000010601     C                   movel     '1'           flgStato          1
057100010615     C                   movel     '0'           flgOk             1
057200010601     C                   z-add     *zeros        cntNonEl         10 0
057300010601     C                   z-add     *zeros        depfil            3 0
057400010601     C                   z-add     *zeros        invfil            3 0
057500010601     C*
057600010601     C                   ENDSR
057700010601     C***
057800010601
057900010601
058000010601
058100010330
058200010330
058300000613     C     *inzsr        BEGSR
058400990910     C*
058500990910     C     *entry        plist
058600990920     C                   parm                    tivlrds
058700990921     C                   parm      wrkesito      esito
058800000724     C                   parm                    prmlit
058900000710     C                   parm                    prmfir
059000010330     C*
059100010330     C* CALCOLA LA DATA CORRENTE
059200010330     C                   time                    wn14             14 0
059300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059400010330     C                   z-add     wn8           g08dat
059500010330     C                   z-add     *zeros        g08inv
059600010330     C                   movel     '0'           g08err
059700010330     C                   call      'XSRDA8'
059800010330     C                   parm                    wlbda8
059900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060000000613     C*
060100000613     C                   ENDSR
060200000613     C***
060300990908
060400050408     Otitvl8p   E            riepilogo         2
060500990915     O                                              'Upload via Internet'
060600990915     O                                           +1 'Traduzione TIVIN00R -'
060700050408     O                                           +1 'FIVABWWR'
060800990915     O                                           +1 'Testate bolle'
060900990915     O          E            riepilogo   2
061000990915     O                       wrkdata
061100990915     O                       wrkora              +1
061200990915     O                       procname            +1
061300990915     O          E            riepilogo   2
061400990915     O                                              'Cliente..................:'
061500010605     O                       VABCCM        z     +1
061600990915     O          E            riepilogo   2
061700990920     O                                              'Riferimento Strategi.....:'
061800990920     O                       vlrhdl              +1
061900990915     O          E            riepilogo   2
062000990915     O                                              'Giusti...................:'
062100971022     O                       §CTROK        2   +  1
062200990915     O          E            riepilogo   2
062300990915     O                                              'Sbagliati e corretti.....:'
062400971022     O                       §CTRMO        2   +  1
062500990915     O          E            riepilogo   2
062600990915     O                                              'Sbagliati e scartati.....:'
062700971022     O                       §CTRNO        2   +  1
062800000613
062900050408     Otitvl8ps  E            testdett          2
063000000613     O                                              'Upload via Internet'
063100000613     O                                           +1 'Traduzione TIVIN00R -'
063200050408     O                                           +1 'FIVABWWR'
063300000613     O                                           +1 'Testate bolle'
063400000616     O          E            testdett    3
063500000613     O                                           +2 'N° rec'
063600000613     O                                           +3 'Anteprima contenuto'
063700000616     O          E            rigadett    2
063800000613     O                       rrnum               +2
063900000621     O                       recko               +3
064000000616     O          E            findett     2
064100000613     O                       wrkdata
064200000613     O                       wrkora              +1
064300000613     O                       procname            +1
064400000616     O          E            findett     2
064500000613     O                                              'Cliente..................:'
064600010605     O                       VABCCM        z     +1
064700000616     O          E            findett     2
064800000613     O                                              'Riferimento Strategi.....:'
064900000613     O                       vlrhdl              +1
065000000616     O          E            findett     2
065100000613     O                                              'Giusti...................:'
065200000613     O                       §CTROK        2   +  1
065300000616     O          E            findett     2
065400000613     O                                              'Sbagliati e corretti.....:'
065500000613     O                       §CTRMO        2   +  1
065600000616     O          E            findett     2
065700000613     O                                              'Sbagliati e scartati.....:'
065800000613     O                       §CTRNO        2   +  1
065900000613** CMD - COMANDI CL
066000050408OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066100050408OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066200050408DLTOVR FILE(TITVL8P TITVL8PS) LVL(*)
066300010606
066400000613
