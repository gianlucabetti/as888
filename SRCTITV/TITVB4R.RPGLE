000100040423      /TITLE Upload via Internet: traduzione in FIVABWWR
000200040423     H*(x UNIPOL/MEIE Cod. 0012031/0012495/0011989/0012137)
000300990908     H dftactgrp(*yes)
000400000313     F*
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021014     FFIVABwwr  O    E             DISK    usropn
000800040423     Ftitvb4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000040423     Ftitvb4ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800040423     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
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
003300040426     D wGiro           s              1  0 INZ(*zeros)
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200000313     D*
004300010330
004400010330
004500010330
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100010601     C*
005200010601     C                   exsr      opeini
005300010605     C                   exsr      rwvab
005400010601     C*
005500010601     C                   seton                                        lr
005600010601
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500010601     C* SE OCCORRE SPEDIRE IN FILIALE
006600010601     C                   if        invfil <> *zeros and
006700010601     C                             flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400010601     C     invfil        chain     azorg01l
007500000724     C                   if        %found
007600000724     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000724     C*
007900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000724     C                   PARM                    depcmd
008300000724     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000613     C*
009700040423     C                   if        not %open(titvb4ps)
009800040423     C                   open      titvb4ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010601     C*--------------------------------------------------------
011200010601     C     ENDELA        BEGSR
011300990915     C*
011400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011500000613     C*
011600040423     C                   if        %open(titvb4ps)
011700000616     C                   except    findett
011800040423     C                   close     titvb4ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700040423     C                   if        not %open(titvb4p)
013800040423     C                   open      titvb4p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300040423     C                   if        %open(titvb4p)
014400040423     C                   close     titvb4p
014500990914     C                   endif
014600000613     C*
014700000613     C                   ENDSR
014800000613     C***
014900000613
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000021014     C                   if        not %open(fivabwwr)
016100021014     C                   open      fivabwwr
016200010330     C                   endif
016300010330     C*
016400010604     C                   clear                   §CTROK
016500010604     C                   clear                   §CTRMO
016600010604     C                   clear                   §CTRNO
016700010330     C*
016800010330     C                   DO        *HIVAL
016900010330     C*
017000010330     C                   READ      tivin00r                               70
017100010618     C*
017200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017300010618     C                   if        vinflg = '1'
017400010618     C                   eval      flgOk = '1'
017500010618     C                   endif
017600010618     C*
017700010330     C                   if        vindta > *blanks
017800010330     C                   add       1             rrnum
017900010330     C*
018000010601     C                   if        *in70 = *off and
018100010330     C                             (vinflg = *blanks
018200010330     C                              or vinflg = '0'
018300010330     C                              or vinflg = '2')
018400010330     C*
018500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018600010711     C                   if        vinflg = *blanks or vinflg = '0'
018700010711     C                   clear                   vinmsg
018800010711     C                   endif
018900010601     C*
019000040426     C* Salto la prima riga xchè contiene le intestazioni dei campi
019100040426     C                   if        wGiro = *zeros
019200040426     C                   add       1             wGiro
019300040426     C                   else
019400010330     C                   exsr      inzvar
019500010330     C                   exsr      defcam
019600010605     C                   exsr      impvab
019700010601     C*
019800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019900010605     C                   eval      depfil = VABLNP
020000010601     C                   exsr      repfil
020100010601     C                   if        depfil = invfil
020200021025     C                   if        vlrpoi = 999
020300021025     C                   MOVE(P)   invfil        VABFGS
020400021025     C                   else
020500021025     C                   MOVE(P)   vlrpoi        VABFGS
020600021025     C                   endif
020700010601     C*
020800010601     C                   exsr      PREELA
020900010601     C*
021000010604     C* Ebbene...
021100010604     C*
021200010604     C  N31              ADD       1             §CTROK            7 0
021300010604     C   32              ADD       1             §CTRMO            7 0
021400010604     C   31              ADD       1             §CTRNO            7 0
021500020205     C*
021600040301     C* Scarico il buffer del FIVAB
021700021014     C  N31              WRITE     FIVAB000
021800010604     C*
021900020722     C                   endif
022000040426     C                   endif
022100020722     C*
022200010604     C                   if        *in31 = *off and
022300010604     C                             *in32 = *off
022400010604     C                   eval      vinflg = '1'
022500010604     C                   else
022600010604     C                   eval      recko = vindta
022700010604     C                   except    rigadett
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010604     C                   endif
023100010604     C*
023200010330     C                   else
023300010330     C                   eval      vinflg = '1'
023400010330     C                   endif
023500010601     C*
023600010601     C  N70              update    tivin000
023700010330     C*
023800010330     C  N70              ENDdo
023900010601     C
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        §ctrno = 0 and
024600010604     C                             §ctrmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100010330     C                   if        §ctrok > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800021014     C                   if        %open(fivabwwr)
026900021014     C                   close     fivabwwr
027000010330     C                   endif
027100010601     C*
027200010601     C                   if        vlrpoi <> 999
027300010601     C                   eval      invfil = vlrpoi
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        §ctrok > 0
027700010601     C                             and invfil > *zeros
027800010330     C                   exsr      invio
027900010330     C                   endif
028000010601     C*
028100010618     C                   if        flgGiro = '1'
028200010601     C                   exsr      endela
028300010618     C                   endif
028400010330     C*
028500010330     C                   ENDSR
028600010330     C***
028700010330
028800010601
028900010601
029000010601
029100010601
029200010330     C*----------------------------------------------------*
029300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400010330     C*----------------------------------------------------*
029500010330     C     INZVAR        BEGSR
029600010330     C*
029700020204     C                   Z-ADD     *zeros        Num5_0            5 0
029800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029900020322     C                   MOVEL     '0'           FlgCAS            1
030000010330     C*
030100010330     C                   ENDSR
030200010330     C*----------------------------------------------------*
030300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030400010330     C*----------------------------------------------------*
030500010330     C     DEFCAM        BEGSR
030600010330     C*
030700020204     C* Inizializzo il buffer del record da scrivere
030800021014     C                   CLEAR                   FIVAB000
030900020204     C* Imposto i valori di default...
031000040426     C                   EVAL      VABCCM = 0012495
031100040426     C                   EVAL      VABLNP = 001
031200040420     C                   EVAL      VABCTR = 000
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
034000021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034100010607     C*----------------------------------------------------*
034200010607     C     IMPVAB        BEGSR
034300010607     C*
034400010607     C                   SETOFF                                       3132
034500010607     C*
034600010607     C* Reperimento campi ALFA
034700040426     C*
034800040426     C* A seconda del CCM e del tipo cliente del cliente stesso
034900040426     C* eseguo differenti considerazioni
035000040426     C                   IF        %trim(%subst(vindta:9:8)) = '1'
035100040426     C                   IF        VABCCM = 0012495 OR
035200040426     C                             VABCCM = 0012031 OR
035300040426     C                             VABCCM = 0011989
035400040426     C                   EVAL      VABRSD='AG.UNIPOL'
035500040426     C                   ENDIF
035600040426     C                   IF        VABCCM = 0012137
035700040426     C                   EVAL      VABRSD='AG.MEIE'
035800040426     C                   ENDIF
035900040426     C                   ENDIF
036000040426     C                   IF        %trim(%subst(vindta:9:8)) = '2' OR
036100040426     C                             %trim(%subst(vindta:9:8)) = '7'
036200040426     C                   EVAL      VABRSD=%trim(%subst(vindta:25:48))
036300040426     C                   ENDIF
036400040426     C*
036500040426     C                   IF        VABRSD = *blanks
036600040426     C                   SETON                                        31
036700040426     C                   EVAL      vinmsg = %trimr(vinmsg)
036800040426     C                             + ' ' + 'VABRSD'
036900040426     C                   ENDIF
037000040426     C*
037100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037300020117     C* ==
037400040426     C                   EVAL      VABIND=%trim(%subst(vindta:73:32))
037500040426     C                   EVAL      VABLOD=%trim(%subst(vindta:113:32))
037600040426     C                   EVAL      VABPRD=%trim(%subst(vindta:145:8))
037700040426     C                   EVAL      VABRMA=%trim(%subst(vindta:17:8))
037800040426     C                   EVAL      VABNOT=%trim(%subst(vindta:169:35))
037900040426     C                   EVAL      VABNT2=%trim(%subst(vindta:169+35:72-35))
038000010607     C*
038100010607     C* Reperimento campi NUMERICI
038200020722     C                   MOVEL     DATCOR        VABAAS
038300020722     C                   MOVE      DATCOR        VABMGS
038400040420     C* NSP/RMN
038500040426     C                   EVAL      PiStr=%trim(%subst(vindta:17:8))
038600020304     C                   EXSR      CHKNUM
038700020304     C                   IF        PiInt=*on
038800040301     C                   Z-ADD     PiVal         VABNSP
038900040301     C                   Z-ADD     PiVal         VABRMN
039000020304     C                   ELSE
039100020722     C                   SETON                                        31
039200020304     C                   Z-ADD     *zeros        VABNSP
039300040301     C                   Z-ADD     *zeros        VABRMN
039400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
039500040301     C                             + ' ' + 'VABNSP VABRMN'
039600020304     C                   ENDIF
039700040420     C* CAD
039800040426     C                   EVAL      PiStr=%trim(%subst(vindta:105:8))
039900010607     C                   EXSR      CHKNUM
040000010607     C                   IF        PiInt=*on
040100020204     C                   Z-ADD     PiVal         Num5_0
040200020204     C                   MOVEL(p)  Num5_0        VABCAD
040300010607     C                   ELSE
040400010607     C                   SETON                                        32
040500020204     C                   EVAL      VABCAD = *zeros
040600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040700020204     C                             + ' ' + 'VABCAD'
040800010607     C                   ENDIF
040900040420     C* NCL
041000040426     C                   EVAL      PiStr=%trim(%subst(vindta:153:8))
041100010607     C                   EXSR      CHKNUM
041200010607     C                   IF        PiInt=*on
041300010607     C                   Z-ADD     PiVal         VABNCL
041400010607     C                   ELSE
041500010607     C                   SETON                                        32
041600010607     C                   Z-ADD     *zeros        VABNCL
041700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041800010607     C                             + ' ' + 'VABNCL'
041900010607     C                   ENDIF
042000040420     C* PKB
042100040426     C                   EVAL      PiStr=%trim(%subst(vindta:161:8))
042200020204     C                   EVAL      PiDecChr = ','
042300010607     C                   EXSR      CHKNUM
042400010607     C                   IF        PiNum=*on
042500040420     C                   Z-ADD     PiVal         VABPKB
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800010607     C                   Z-ADD     *zeros        VABPKB
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000010607     C                             + ' ' + 'VABPKB'
043100010607     C                   ENDIF
043200020322     C*
043300020322     C* Considerazioni finali su CBO/CAS
043400020322     C                   IF        FlgCAS = '1'
043500040426     C                   EVAL      VABCBO = '1'
043600040420     C                   ELSE
043700040426     C                   EVAL      VABCBO = '2'
043800020322     C                   ENDIF
043900020204     C*
044000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044100020204     C                   EXSR      CHKIMPDIV
044200020204     C*
044300010607     C                   ENDSR
044400010607     C*----------------------------------------------------*
044500010601
044600020204
044700020204     C*----------------------------------------------------*
044800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044900020204     C*----------------------------------------------------*
045000020204     C     CHKIMPDIV     BEGSR
045100020204     C*
045200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045300020204     C                   Z-ADD     *zeros        wrkDec            9 9
045400020204     C*
045500020204     C* Come prima cosa effettuo considerazioni sulla divisa
045600020204     C                   IF        vabIAS > *zeros
045700020204     C                   IF        vabVAS <> 'EUR'
045800020204     C                   EVAL      vabVAS =  'ITL'
045900020204     C                   ENDIF
046000020204     C                   ENDIF
046100020204     C*
046200020204     C                   IF        vabCAS > *zeros
046300020204     C                   IF        vabVCA <> 'EUR'
046400020204     C                   EVAL      vabVCA =  'ITL'
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C                   IF        vabVMD > *zeros
046900020321     C                   IF        vabVAD <> 'EUR'
047000020204     C                   EVAL      vabVAD =  'ITL'
047100020204     C                   ENDIF
047200020204     C                   ENDIF
047300020204     C*
047400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047500020204     C                   Z-ADD     vabIAS        wrkDec
047600020204     C                   IF        wrkDec > *zeros
047700020204     C                   IF        vabVAS = 'ITL'
047800020204     C                   EVAL      vabIAS = *zeros
047900020204     C                   ENDIF
048000020204     C                   ENDIF
048100020204     C*
048200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048300020204     C                   Z-ADD     vabCAS        wrkDec
048400020204     C                   IF        wrkDec > *zeros
048500020204     C                   IF        vabVCA = 'ITL'
048600020204     C                   EVAL      vabCAS = *zeros
048700020204     C                   ENDIF
048800020204     C                   ENDIF
048900020204     C*
049000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049100020204     C                   Z-ADD     vabVMD        wrkDec
049200020204     C                   IF        wrkDec > *zeros
049300020204     C                   IF        vabVAD = 'ITL'
049400020204     C                   EVAL      vabVMD = *zeros
049500020204     C                   ENDIF
049600020204     C                   ENDIF
049700020204     C*
049800020204     C                   ENDSR
049900020204     C***
050000020204
050100010330
050200010330
050300010330     C*----------------------------------------------------*
050400010330     C*  CONTROLLO NUMERICITA' CAMPI
050500010330     C*----------------------------------------------------*
050600010330     C     CHKNUM        BEGSR
050700010330     C*
050800010606     C                   IF        PiDecChr = *blanks
050900010606     C                   EVAL      PiDecChr = ','
051000010606     C                   ENDIF
051100010606     C*
051200010606     C                   CALL(e)   'ISNUMERIC'
051300010330     C                   PARM                    PiStr            30
051400010606     C                   PARM                    PiDecChr          1
051500010330     C                   PARM      *ZEROS        PiVal            30 9
051600010330     C                   PARM      '0'           PiInt             1
051700010330     C                   PARM      '0'           PiNum             1
051800010330     C                   IF        %error
051900010606     C                   EVAL      PiNum=*off
052000010330     C                   ENDIF
052100010330     C*
052200010330     C                   ENDSR
052300010330     C***
052400010330
052500010601
052600010601
052700010601
052800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052900010601     C     repfil        BEGSR
053000010601     C*
053100010601     C                   if        invfil = *zeros and
053200010601     C                             depfil > *zeros and
053300010629     C                             (vinflg = *blanks or
053400010629     C                              vinflg = *zeros)
053500010601     C
053600010601     C                   eval      invfil = depfil
053700010601     C                   endif
053800010601     C*
053900010601     C                   if        depfil <> invfil and
054000010601     C                             invfil > *zeros
054100010601     C                   eval      flgMulti = '1'
054200010601     C                   if        vinflg = *blanks
054300010601     C                   add       1             cntNonEl
054400010601     C                   endif
054500010601     C                   endif
054600010601     C*
054700010601     C                   if        vinflg = '2'
054800010601     C                   eval      flgStato = '2'
054900010601     C                   endif
055000010601     C*
055100010601     C                   ENDSR
055200010601     C***
055300010601
055400010601
055500010601
055600010330
055700010330
055800010330
055900990920      /TITLE Invio dei dati al punto operativo.
056000000613     C     invio         BEGSR
056100990920     C*
056200990920     C                   reset                   dscmz
056300010601     C                   move      invfil        cmzdst
056400990920     C                   eval      cmzfld = vlrfou
056500990920     C                   eval      cmzmbd = vlrhdl
056600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056700000710     C                   if        prmfir = *blanks
056800021014     C                   eval      cmzfla = 'FIVAB00F'
056900021014     C                   eval      cmzmba = 'FIVAB00F'
057000000710     C                   else
057100000710     C                   eval      cmzfla = prmfir
057200000710     C                   eval      cmzmba = prmfir
057300000710     C                   endif
057400990920     C                   eval      cmznrr = *zeros
057500990920     C                   move      §ctrok        cmznrr
057600021018     C                   eval      cmzlba = vlrfl1
057700990920     C                   call(e)   'TIS711C'
057800990920     C                   parm                    dscmz
057900990921     C                   parm      *blanks       esito
058000990920     C                   if        %error
058100990920     C                             or cmzerr = '1'
058200990921     C                             or esito  = '1'
058300000710     C                   eval      wrkesito = '3'
058400990920     C                   endif
058500990920     C*
058600000613     C                   ENDSR
058700990910
058800010601
058900010601
059000010601
059100010601
059200010601      /TITLE Invio dei dati al punto operativo.
059300010601     C     opeini        BEGSR
059400010601     C*
059500010601     C* Inizializzo flag e contatori operativi
059600010601     C                   movel     '0'           flgGiro           1
059700010601     C                   movel     '0'           flgMulti          1
059800010601     C                   movel     '1'           flgStato          1
059900010615     C                   movel     '0'           flgOk             1
060000010601     C                   z-add     *zeros        cntNonEl         10 0
060100010601     C                   z-add     *zeros        depfil            3 0
060200010601     C                   z-add     *zeros        invfil            3 0
060300010601     C*
060400010601     C                   ENDSR
060500010601     C***
060600010601
060700010601
060800010601
060900010330
061000010330
061100000613     C     *inzsr        BEGSR
061200990910     C*
061300990910     C     *entry        plist
061400990920     C                   parm                    tivlrds
061500990921     C                   parm      wrkesito      esito
061600000724     C                   parm                    prmlit
061700000710     C                   parm                    prmfir
061800010330     C*
061900010330     C* CALCOLA LA DATA CORRENTE
062000010330     C                   time                    wn14             14 0
062100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062200010330     C                   z-add     wn8           g08dat
062300010330     C                   z-add     *zeros        g08inv
062400010330     C                   movel     '0'           g08err
062500010330     C                   call      'XSRDA8'
062600010330     C                   parm                    wlbda8
062700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062800000613     C*
062900000613     C                   ENDSR
063000000613     C***
063100990908
063200040423     Otitvb4p   E            riepilogo         2
063300990915     O                                              'Upload via Internet'
063400990915     O                                           +1 'Traduzione TIVIN00R -'
063500021014     O                                           +1 'FIVABWWR'
063600990915     O                                           +1 'Testate bolle'
063700990915     O          E            riepilogo   2
063800990915     O                       wrkdata
063900990915     O                       wrkora              +1
064000990915     O                       procname            +1
064100990915     O          E            riepilogo   2
064200990915     O                                              'Cliente..................:'
064300010605     O                       VABCCM        z     +1
064400990915     O          E            riepilogo   2
064500990920     O                                              'Riferimento Strategi.....:'
064600990920     O                       vlrhdl              +1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Giusti...................:'
064900971022     O                       §CTROK        2   +  1
065000990915     O          E            riepilogo   2
065100990915     O                                              'Sbagliati e corretti.....:'
065200971022     O                       §CTRMO        2   +  1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Sbagliati e scartati.....:'
065500971022     O                       §CTRNO        2   +  1
065600000613
065700040423     Otitvb4ps  E            testdett          2
065800000613     O                                              'Upload via Internet'
065900000613     O                                           +1 'Traduzione TIVIN00R -'
066000021014     O                                           +1 'FIVABWWR'
066100000613     O                                           +1 'Testate bolle'
066200000616     O          E            testdett    3
066300000613     O                                           +2 'N° rec'
066400000613     O                                           +3 'Anteprima contenuto'
066500000616     O          E            rigadett    2
066600000613     O                       rrnum               +2
066700000621     O                       recko               +3
066800000616     O          E            findett     2
066900000613     O                       wrkdata
067000000613     O                       wrkora              +1
067100000613     O                       procname            +1
067200000616     O          E            findett     2
067300000613     O                                              'Cliente..................:'
067400010605     O                       VABCCM        z     +1
067500000616     O          E            findett     2
067600000613     O                                              'Riferimento Strategi.....:'
067700000613     O                       vlrhdl              +1
067800000616     O          E            findett     2
067900000613     O                                              'Giusti...................:'
068000000613     O                       §CTROK        2   +  1
068100000616     O          E            findett     2
068200000613     O                                              'Sbagliati e corretti.....:'
068300000613     O                       §CTRMO        2   +  1
068400000616     O          E            findett     2
068500000613     O                                              'Sbagliati e scartati.....:'
068600000613     O                       §CTRNO        2   +  1
068700000613** CMD - COMANDI CL
068800040423OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068900040423OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069000040423DLTOVR FILE(TITVB4P TITVB4PS) LVL(*)
069100010606
069200000613
