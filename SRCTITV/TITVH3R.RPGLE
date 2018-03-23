000100050125      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ALPINE Cod. 0500821)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700050125     Ftitvh2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050125     Ftitvh2ps  O    f  198        PRINTER usropn
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
009400050125     C                   if        not %open(titvh2ps)
009500050125     C                   open      titvh2ps
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
011300050125     C                   if        %open(titvh2ps)
011400000616     C                   except    findett
011500050125     C                   close     titvh2ps
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
013400050125     C                   if        not %open(titvh2p)
013500050125     C                   open      titvh2p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000050125     C                   if        %open(titvh2p)
014100050125     C                   close     titvh2p
014200990914     C                   endif
014300000613     C*
014400000613     C                   ENDSR
014500000613     C***
014600000613
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700021014     C                   if        not %open(fivabwwr)
015800021014     C                   open      fivabwwr
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
022300010604     C*
022400010330     C                   else
022500010330     C                   eval      vinflg = '1'
022600010330     C                   endif
022700010601     C*
022800010601     C  N70              update    tivin000
022900010330     C*
023000010330     C  N70              ENDdo
023100010601     C
023200010601     C*
023300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023400010601     C                   if        cntNonEl = *zeros or
023500010601     C                             flgMulti = '0'
023600010330     C* Se non ci sono record con errori ...
023700010601     C                   if        §ctrno = 0 and
023800010604     C                             §ctrmo = 0 and
023900010601     C                             flgStato <> '2'
024000010330     C* ... restituisco esito OK.
024100010330     C                   eval      wrkesito = '0'
024200010330     C                   else
024300010330     C                   if        §ctrok > 0
024400010330     C                   eval      wrkesito = '1'
024500010330     C                   else
024600010615     C                   if        flgOk = '0'
024700010615     C                   eval      wrkesito = '2'
024800010615     C                   else
024900010615     C                   eval      wrkesito = '6'
025000010615     C                   endif
025100010330     C                   endif
025200010330     C                   endif
025300010601     C                   else
025400010601     C                   eval      wrkesito = '9'
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        %open(tivin00r)
025800010330     C                   close     tivin00r
025900010330     C                   endif
026000021014     C                   if        %open(fivabwwr)
026100021014     C                   close     fivabwwr
026200010330     C                   endif
026300010601     C*
026400010601     C                   if        vlrpoi <> 999
026500010601     C                   eval      invfil = vlrpoi
026600010601     C                   endif
026700010330     C*
026800010330     C                   if        §ctrok > 0
026900010601     C                             and invfil > *zeros
027000010330     C                   exsr      invio
027100010330     C                   endif
027200010601     C*
027300010618     C                   if        flgGiro = '1'
027400010601     C                   exsr      endela
027500010618     C                   endif
027600010330     C*
027700010330     C                   ENDSR
027800010330     C***
027900010330
028000010601
028100010601
028200010601
028300010601
028400010330     C*----------------------------------------------------*
028500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028600010330     C*----------------------------------------------------*
028700010330     C     INZVAR        BEGSR
028800010330     C*
028900020204     C                   Z-ADD     *zeros        Num5_0            5 0
029000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029100020322     C                   MOVEL     '0'           FlgCAS            1
029200010330     C*
029300010330     C                   ENDSR
029400010330     C*----------------------------------------------------*
029500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029600010330     C*----------------------------------------------------*
029700010330     C     DEFCAM        BEGSR
029800010330     C*
029900020204     C* Inizializzo il buffer del record da scrivere
030000021014     C                   CLEAR                   FIVAB000
030100020204     C* Imposto i valori di default...
030200050125     C                   EVAL      VABCCM = 0500821
030300050125     C                   EVAL      VABLNP = 050
030400040713     C                   EVAL      VABCTR = 000
030500041108     C                   EVAL      VABCBO = '1'
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
033300021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033400010607     C*----------------------------------------------------*
033500010607     C     IMPVAB        BEGSR
033600010607     C*
033700010607     C                   SETOFF                                       3132
033800010607     C*
033900010607     C* Reperimento campi ALFA
034000010607     C*
034100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034200050128     C                   EVAL      VABRSD=%trim(%subst(vindta:151:30))
034300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034500020117     C* ==
034600050128     C                   EVAL      VABRD2=%trim(%subst(vindta:181:28))
034700050128     C                   EVAL      VABIND=%trim(%subst(vindta:212:30))
034800050128     C                   EVAL      VABLOD=%trim(%subst(vindta:248:23))
034900050128     C                   EVAL      VABPRD=%trim(%subst(vindta:271:2))
035000050128     C                   EVAL      VABRMA=%trim(%subst(vindta:1:13))
035100050128     C                   EVAL      VABNOT=%trim(%subst(vindta:307:35))
035200050128     C                   EVAL      VABNT2=%trim(%subst(vindta:307+35:113-35))
035300010607     C*
035400010607     C* Reperimento campi NUMERICI
035500020722     C                   MOVEL     DATCOR        VABAAS
035600020722     C                   MOVE      DATCOR        VABMGS
035700040420     C* NSP/RMN
035800050128     C                   EVAL      PiStr=%trim(%subst(vindta:9:5))
035900020304     C                   EXSR      CHKNUM
036000020304     C                   IF        PiInt=*on
036100040301     C                   Z-ADD     PiVal         VABNSP
036200040301     C                   Z-ADD     PiVal         VABRMN
036300020304     C                   ELSE
036400020722     C                   SETON                                        31
036500020304     C                   Z-ADD     *zeros        VABNSP
036600040301     C                   Z-ADD     *zeros        VABRMN
036700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036800040301     C                             + ' ' + 'VABNSP VABRMN'
036900020304     C                   ENDIF
037000050128     C* CAD
037100050128     C                   EVAL      PiStr=%trim(%subst(vindta:243:5))
037200050128     C                   EXSR      CHKNUM
037300050128     C                   IF        PiInt=*on
037400050128     C                   Z-ADD     PiVal         Num5_0
037500050128     C                   MOVEL(p)  Num5_0        VABCAD
037600050128     C                   ELSE
037700050128     C                   SETON                                        32
037800050128     C                   EVAL      VABCAD = *zeros
037900050128     C                   EVAL      vinmsg = %trimr(vinmsg)
038000050128     C                             + ' ' + 'VABCAD'
038100050128     C                   ENDIF
038200040420     C* NCL
038300050128     C                   EVAL      PiStr=%trim(%subst(vindta:281:3))
038400010607     C                   EXSR      CHKNUM
038500010607     C                   IF        PiInt=*on
038600010607     C                   Z-ADD     PiVal         VABNCL
038700010607     C                   ELSE
038800010607     C                   SETON                                        32
038900010607     C                   Z-ADD     *zeros        VABNCL
039000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039100010607     C                             + ' ' + 'VABNCL'
039200010607     C                   ENDIF
039300040420     C* PKB
039400050128     C                   EVAL      PiStr=%trim(%subst(vindta:288:7))
039500010607     C                   EXSR      CHKNUM
039600010607     C                   IF        PiNum=*on
039700050114     C                   Z-ADD     PiVal         VABPKB
039800010607     C                   ELSE
039900010607     C                   SETON                                        32
040000010607     C                   Z-ADD     *zeros        VABPKB
040100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040200010607     C                             + ' ' + 'VABPKB'
040300010607     C                   ENDIF
040400050125     C* VLB
040500050128     C                   EVAL      PiStr=%trim(%subst(vindta:300:6))
040600050125     C                   EXSR      CHKNUM
040700050125     C                   IF        PiNum=*on
040800050125     C                   Z-ADD     PiVal         VABVLB
040900050125     C                   ELSE
041000050125     C                   SETON                                        32
041100050125     C                   Z-ADD     *zeros        VABVLB
041200050125     C                   EVAL      vinmsg = %trimr(vinmsg)
041300050125     C                             + ' ' + 'VABVLB'
041400050125     C                   ENDIF
041500020322     C*
041600020322     C* Considerazioni finali su CBO/CAS
041700020322     C                   IF        FlgCAS = '1'
041800040713     C                   IF        VABCBO = '1'
041900020322     C                   EVAL      VABCBO = '4'
042000040713     C                   ENDIF
042100040713     C                   IF        VABCBO = '2'
042200040713     C                   EVAL      VABCBO = '6'
042300040713     C                   ENDIF
042400020322     C                   ENDIF
042500020204     C*
042600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042700020204     C                   EXSR      CHKIMPDIV
042800020204     C*
042900010607     C                   ENDSR
043000010607     C*----------------------------------------------------*
043100010601
043200020204
043300020204     C*----------------------------------------------------*
043400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043500020204     C*----------------------------------------------------*
043600020204     C     CHKIMPDIV     BEGSR
043700020204     C*
043800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043900020204     C                   Z-ADD     *zeros        wrkDec            9 9
044000020204     C*
044100020204     C* Come prima cosa effettuo considerazioni sulla divisa
044200020204     C                   IF        vabIAS > *zeros
044300020204     C                   IF        vabVAS <> 'EUR'
044400020204     C                   EVAL      vabVAS =  'ITL'
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C                   IF        vabCAS > *zeros
044900020204     C                   IF        vabVCA <> 'EUR'
045000020204     C                   EVAL      vabVCA =  'ITL'
045100020204     C                   ENDIF
045200020204     C                   ENDIF
045300020204     C*
045400020204     C                   IF        vabVMD > *zeros
045500020321     C                   IF        vabVAD <> 'EUR'
045600020204     C                   EVAL      vabVAD =  'ITL'
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046100020204     C                   Z-ADD     vabIAS        wrkDec
046200020204     C                   IF        wrkDec > *zeros
046300020204     C                   IF        vabVAS = 'ITL'
046400020204     C                   EVAL      vabIAS = *zeros
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046900020204     C                   Z-ADD     vabCAS        wrkDec
047000020204     C                   IF        wrkDec > *zeros
047100020204     C                   IF        vabVCA = 'ITL'
047200020204     C                   EVAL      vabCAS = *zeros
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047700020204     C                   Z-ADD     vabVMD        wrkDec
047800020204     C                   IF        wrkDec > *zeros
047900020204     C                   IF        vabVAD = 'ITL'
048000020204     C                   EVAL      vabVMD = *zeros
048100020204     C                   ENDIF
048200020204     C                   ENDIF
048300020204     C*
048400020204     C                   ENDSR
048500020204     C***
048600020204
048700010330
048800010330
048900010330     C*----------------------------------------------------*
049000010330     C*  CONTROLLO NUMERICITA' CAMPI
049100010330     C*----------------------------------------------------*
049200010330     C     CHKNUM        BEGSR
049300010330     C*
049400010606     C                   IF        PiDecChr = *blanks
049500050125     C                   EVAL      PiDecChr = '.'
049600010606     C                   ENDIF
049700010606     C*
049800010606     C                   CALL(e)   'ISNUMERIC'
049900010330     C                   PARM                    PiStr            30
050000010606     C                   PARM                    PiDecChr          1
050100010330     C                   PARM      *ZEROS        PiVal            30 9
050200010330     C                   PARM      '0'           PiInt             1
050300010330     C                   PARM      '0'           PiNum             1
050400010330     C                   IF        %error
050500010606     C                   EVAL      PiNum=*off
050600010330     C                   ENDIF
050700010330     C*
050800010330     C                   ENDSR
050900010330     C***
051000010330
051100010601
051200010601
051300010601
051400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051500010601     C     repfil        BEGSR
051600010601     C*
051700010601     C                   if        invfil = *zeros and
051800010601     C                             depfil > *zeros and
051900010629     C                             (vinflg = *blanks or
052000010629     C                              vinflg = *zeros)
052100010601     C
052200010601     C                   eval      invfil = depfil
052300010601     C                   endif
052400010601     C*
052500010601     C                   if        depfil <> invfil and
052600010601     C                             invfil > *zeros
052700010601     C                   eval      flgMulti = '1'
052800010601     C                   if        vinflg = *blanks
052900010601     C                   add       1             cntNonEl
053000010601     C                   endif
053100010601     C                   endif
053200010601     C*
053300010601     C                   if        vinflg = '2'
053400010601     C                   eval      flgStato = '2'
053500010601     C                   endif
053600010601     C*
053700010601     C                   ENDSR
053800010601     C***
053900010601
054000010601
054100010601
054200010330
054300010330
054400010330
054500990920      /TITLE Invio dei dati al punto operativo.
054600000613     C     invio         BEGSR
054700990920     C*
054800990920     C                   reset                   dscmz
054900010601     C                   move      invfil        cmzdst
055000990920     C                   eval      cmzfld = vlrfou
055100990920     C                   eval      cmzmbd = vlrhdl
055200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055300000710     C                   if        prmfir = *blanks
055400021014     C                   eval      cmzfla = 'FIVAB00F'
055500021014     C                   eval      cmzmba = 'FIVAB00F'
055600000710     C                   else
055700000710     C                   eval      cmzfla = prmfir
055800000710     C                   eval      cmzmba = prmfir
055900000710     C                   endif
056000990920     C                   eval      cmznrr = *zeros
056100990920     C                   move      §ctrok        cmznrr
056200021018     C                   eval      cmzlba = vlrfl1
056300990920     C                   call(e)   'TIS711C'
056400990920     C                   parm                    dscmz
056500990921     C                   parm      *blanks       esito
056600990920     C                   if        %error
056700990920     C                             or cmzerr = '1'
056800990921     C                             or esito  = '1'
056900000710     C                   eval      wrkesito = '3'
057000990920     C                   endif
057100990920     C*
057200000613     C                   ENDSR
057300990910
057400010601
057500010601
057600010601
057700010601
057800010601      /TITLE Invio dei dati al punto operativo.
057900010601     C     opeini        BEGSR
058000010601     C*
058100010601     C* Inizializzo flag e contatori operativi
058200010601     C                   movel     '0'           flgGiro           1
058300010601     C                   movel     '0'           flgMulti          1
058400010601     C                   movel     '1'           flgStato          1
058500010615     C                   movel     '0'           flgOk             1
058600010601     C                   z-add     *zeros        cntNonEl         10 0
058700010601     C                   z-add     *zeros        depfil            3 0
058800010601     C                   z-add     *zeros        invfil            3 0
058900010601     C*
059000010601     C                   ENDSR
059100010601     C***
059200010601
059300010601
059400010601
059500010330
059600010330
059700000613     C     *inzsr        BEGSR
059800990910     C*
059900990910     C     *entry        plist
060000990920     C                   parm                    tivlrds
060100990921     C                   parm      wrkesito      esito
060200000724     C                   parm                    prmlit
060300000710     C                   parm                    prmfir
060400010330     C*
060500010330     C* CALCOLA LA DATA CORRENTE
060600010330     C                   time                    wn14             14 0
060700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060800010330     C                   z-add     wn8           g08dat
060900010330     C                   z-add     *zeros        g08inv
061000010330     C                   movel     '0'           g08err
061100010330     C                   call      'XSRDA8'
061200010330     C                   parm                    wlbda8
061300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061400000613     C*
061500000613     C                   ENDSR
061600000613     C***
061700990908
061800050125     Otitvh2p   E            riepilogo         2
061900990915     O                                              'Upload via Internet'
062000990915     O                                           +1 'Traduzione TIVIN00R -'
062100021014     O                                           +1 'FIVABWWR'
062200990915     O                                           +1 'Testate bolle'
062300990915     O          E            riepilogo   2
062400990915     O                       wrkdata
062500990915     O                       wrkora              +1
062600990915     O                       procname            +1
062700990915     O          E            riepilogo   2
062800990915     O                                              'Cliente..................:'
062900010605     O                       VABCCM        z     +1
063000990915     O          E            riepilogo   2
063100990920     O                                              'Riferimento Strategi.....:'
063200990920     O                       vlrhdl              +1
063300990915     O          E            riepilogo   2
063400990915     O                                              'Giusti...................:'
063500971022     O                       §CTROK        2   +  1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Sbagliati e corretti.....:'
063800971022     O                       §CTRMO        2   +  1
063900990915     O          E            riepilogo   2
064000990915     O                                              'Sbagliati e scartati.....:'
064100971022     O                       §CTRNO        2   +  1
064200000613
064300050125     Otitvh2ps  E            testdett          2
064400000613     O                                              'Upload via Internet'
064500000613     O                                           +1 'Traduzione TIVIN00R -'
064600021014     O                                           +1 'FIVABWWR'
064700000613     O                                           +1 'Testate bolle'
064800000616     O          E            testdett    3
064900000613     O                                           +2 'N° rec'
065000000613     O                                           +3 'Anteprima contenuto'
065100000616     O          E            rigadett    2
065200000613     O                       rrnum               +2
065300000621     O                       recko               +3
065400000616     O          E            findett     2
065500000613     O                       wrkdata
065600000613     O                       wrkora              +1
065700000613     O                       procname            +1
065800000616     O          E            findett     2
065900000613     O                                              'Cliente..................:'
066000010605     O                       VABCCM        z     +1
066100000616     O          E            findett     2
066200000613     O                                              'Riferimento Strategi.....:'
066300000613     O                       vlrhdl              +1
066400000616     O          E            findett     2
066500000613     O                                              'Giusti...................:'
066600000613     O                       §CTROK        2   +  1
066700000616     O          E            findett     2
066800000613     O                                              'Sbagliati e corretti.....:'
066900000613     O                       §CTRMO        2   +  1
067000000616     O          E            findett     2
067100000613     O                                              'Sbagliati e scartati.....:'
067200000613     O                       §CTRNO        2   +  1
067300000613** CMD - COMANDI CL
067400050125OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067500050125OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067600050125DLTOVR FILE(TITVH2P TITVH2PS) LVL(*)
067700010606
067800000613
