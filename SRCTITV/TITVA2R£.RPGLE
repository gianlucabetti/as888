000100040301      /TITLE Upload via Internet: traduzione in FIVABWWR. (x PARFUMES GIVENCHY Cod. 0890845)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700040301     Ftitva2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040301     Ftitva2ps  O    f  198        PRINTER usropn
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
009500040301     C                   if        not %open(titva2ps)
009600040301     C                   open      titva2ps
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
011400040301     C                   if        %open(titva2ps)
011500000616     C                   except    findett
011600040301     C                   close     titva2ps
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
013500040301     C                   if        not %open(titva2p)
013600040301     C                   open      titva2p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100040301     C                   if        %open(titva2p)
014200040301     C                   close     titva2p
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
018800010330     C                   exsr      inzvar
018900010330     C                   exsr      defcam
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
029300040301     C                   MOVEL     *all'0'       wDCR              8
029400010330     C*
029500010330     C                   ENDSR
029600010330     C*----------------------------------------------------*
029700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029800010330     C*----------------------------------------------------*
029900010330     C     DEFCAM        BEGSR
030000010330     C*
030100020204     C* Inizializzo il buffer del record da scrivere
030200021014     C                   CLEAR                   FIVAB000
030300020204     C* Imposto i valori di default...
030400040408     C                   EVAL      VABCCM = 0892753
030500040301     C                   EVAL      VABLNP = 089
030600040301     C                   EVAL      VABCTR = 001
030700040301     C                   EVAL      VABTSP = 'C'
030800050127     C                   EVAL      VABCTM = '7Q'
030900020204     C* ... e poi verifico se sono stati passati come parametri
031000020204     C                   IF        vlrppt > *blanks
031100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031300020204     C                   EXSR      CHKNUM
031400020204     C                   IF        PiInt=*on
031500020204     C                   Z-ADD     PiVal         VABCCM
031600020204     C                   ENDIF
031700040301     C                   ENDIF
031800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032000020204     C                   EXSR      CHKNUM
032100020204     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         VABLNP
032300020204     C                   ENDIF
032400040301     C                   ENDIF
032500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABCTR
033000040301     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C                   ENDIF
033300020204     C*
033400010330     C                   ENDSR
033500010607     C*----------------------------------------------------*
033600021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033700010607     C*----------------------------------------------------*
033800010607     C     IMPVAB        BEGSR
033900010607     C*
034000010607     C                   SETOFF                                       3132
034100010607     C*
034200010607     C* Reperimento campi ALFA
034300010607     C*
034400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034500040301     C                   EVAL      VABRSD=%trim(%subst(vindta:9:30))
034600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034800020117     C* ==
034900040301     C                   EVAL      VABIND=%trim(%subst(vindta:39:30))
035000040301     C                   EVAL      VABLOD=%trim(%subst(vindta:69:20))
035100040301     C                   EVAL      VABPRD=%trim(%subst(vindta:94:2))
035200040614     C***                EVAL      VABRMA=%trim(%subst(vindta:199:10))
035300040614     C                   EVAL      VABRMA=%trim(%subst(vindta:97:10))
035400050318     C                   EVAL      VABNT2=%trim(%subst(vindta:113:16))
035500040618     C*
035600040618     C* Verifico particolarecondizione che condiziona la valorizzazoine dell'RMA
035700040618     C                   IF        %subst(vindta:6:2) <> *blanks
035800040618     C                   EVAL      %subst(VABRMA:%len(%trim(VABRMA))+1:
035900040618     C                             %size(VABRMA)-%len(%trim(VABRMA))) = *all'*'
036000040618     C                   ENDIF
036100040618     C*
036200040301     C                   EVAL      VABNOT=%trim(%subst(vindta:153:30))
036300040301     C                   IF        %subst(vindta:96:1) = '9'
036400040301     C                   EVAL      VABTCR = 'P'
036500040301     C                   ENDIF
036600040301     C                   EVAL      wDCR = %subst(vindta:183+8-4:4)+
036700040301     C                                    %subst(vindta:183+8-4-2:2)+
036800040301     C                                    %subst(vindta:183:2)
036900040301     C                   MOVE(P)   wDCR          VABDCR
037000010607     C*
037100010607     C* Reperimento campi NUMERICI
037200020722     C                   MOVEL     DATCOR        VABAAS
037300020722     C                   MOVE      DATCOR        VABMGS
037400020722     C*
037500040301     C                   EVAL      PiStr=%trim(%subst(vindta:97:10))
037600020304     C                   EXSR      CHKNUM
037700020304     C                   IF        PiInt=*on
037800040301     C                   Z-ADD     PiVal         VABNSP
037900040301     C                   Z-ADD     PiVal         VABRMN
038000020304     C                   ELSE
038100020722     C                   SETON                                        31
038200020304     C                   Z-ADD     *zeros        VABNSP
038300040301     C                   Z-ADD     *zeros        VABRMN
038400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038500040301     C                             + ' ' + 'VABNSP VABRMN'
038600020304     C                   ENDIF
038700020304     C*
038800040301     C                   EVAL      PiStr=%trim(%subst(vindta:89:5))
038900010607     C                   EXSR      CHKNUM
039000010607     C                   IF        PiInt=*on
039100020204     C                   Z-ADD     PiVal         Num5_0
039200020204     C                   MOVEL(p)  Num5_0        VABCAD
039300010607     C                   ELSE
039400010607     C                   SETON                                        32
039500020204     C                   EVAL      VABCAD = *zeros
039600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039700020204     C                             + ' ' + 'VABCAD'
039800010607     C                   ENDIF
039900010607     C*
040000040301     C                   EVAL      PiStr=%trim(%subst(vindta:129:5))
040100010607     C                   EXSR      CHKNUM
040200010607     C                   IF        PiInt=*on
040300010607     C                   Z-ADD     PiVal         VABNCL
040400010607     C                   ELSE
040500010607     C                   SETON                                        32
040600010607     C                   Z-ADD     *zeros        VABNCL
040700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040800010607     C                             + ' ' + 'VABNCL'
040900010607     C                   ENDIF
041000020304     C*
041100040301     C                   EVAL      PiStr=%trim(%subst(vindta:135:6))
041200020204     C                   EVAL      PiDecChr = ','
041300010607     C                   EXSR      CHKNUM
041400010607     C                   IF        PiNum=*on
041500040617     C                   EVAL      VABPKB = PiVal/1000                          * gestisco 3 dec.
041600010607     C                   ELSE
041700010607     C                   SETON                                        32
041800010607     C                   Z-ADD     *zeros        VABPKB
041900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042000010607     C                             + ' ' + 'VABPKB'
042100010607     C                   ENDIF
042200020322     C*
042300020322     C* Considerazioni finali su CBO/CAS
042400020322     C                   IF        FlgCAS = '1'
042500020322     C                   EVAL      VABCBO = '4'
042600020322     C                   ELSE
042700020322     C                   EVAL      VABCBO = '1'
042800020322     C                   ENDIF
042900020204     C*
043000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043100020204     C                   EXSR      CHKIMPDIV
043200020204     C*
043300010607     C                   ENDSR
043400010607     C*----------------------------------------------------*
043500010601
043600020204
043700020204     C*----------------------------------------------------*
043800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043900020204     C*----------------------------------------------------*
044000020204     C     CHKIMPDIV     BEGSR
044100020204     C*
044200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044300020204     C                   Z-ADD     *zeros        wrkDec            9 9
044400020204     C*
044500020204     C* Come prima cosa effettuo considerazioni sulla divisa
044600020204     C                   IF        vabIAS > *zeros
044700020204     C                   IF        vabVAS <> 'EUR'
044800020204     C                   EVAL      vabVAS =  'ITL'
044900020204     C                   ENDIF
045000020204     C                   ENDIF
045100020204     C*
045200020204     C                   IF        vabCAS > *zeros
045300020204     C                   IF        vabVCA <> 'EUR'
045400020204     C                   EVAL      vabVCA =  'ITL'
045500020204     C                   ENDIF
045600020204     C                   ENDIF
045700020204     C*
045800020204     C                   IF        vabVMD > *zeros
045900020321     C                   IF        vabVAD <> 'EUR'
046000020204     C                   EVAL      vabVAD =  'ITL'
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046500020204     C                   Z-ADD     vabIAS        wrkDec
046600020204     C                   IF        wrkDec > *zeros
046700020204     C                   IF        vabVAS = 'ITL'
046800020204     C                   EVAL      vabIAS = *zeros
046900020204     C                   ENDIF
047000020204     C                   ENDIF
047100020204     C*
047200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047300020204     C                   Z-ADD     vabCAS        wrkDec
047400020204     C                   IF        wrkDec > *zeros
047500020204     C                   IF        vabVCA = 'ITL'
047600020204     C                   EVAL      vabCAS = *zeros
047700020204     C                   ENDIF
047800020204     C                   ENDIF
047900020204     C*
048000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048100020204     C                   Z-ADD     vabVMD        wrkDec
048200020204     C                   IF        wrkDec > *zeros
048300020204     C                   IF        vabVAD = 'ITL'
048400020204     C                   EVAL      vabVMD = *zeros
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C                   ENDSR
048900020204     C***
049000020204
049100010330
049200010330
049300010330     C*----------------------------------------------------*
049400010330     C*  CONTROLLO NUMERICITA' CAMPI
049500010330     C*----------------------------------------------------*
049600010330     C     CHKNUM        BEGSR
049700010330     C*
049800010606     C                   IF        PiDecChr = *blanks
049900010606     C                   EVAL      PiDecChr = ','
050000010606     C                   ENDIF
050100010606     C*
050200010606     C                   CALL(e)   'ISNUMERIC'
050300010330     C                   PARM                    PiStr            30
050400010606     C                   PARM                    PiDecChr          1
050500010330     C                   PARM      *ZEROS        PiVal            30 9
050600010330     C                   PARM      '0'           PiInt             1
050700010330     C                   PARM      '0'           PiNum             1
050800010330     C                   IF        %error
050900010606     C                   EVAL      PiNum=*off
051000010330     C                   ENDIF
051100010330     C*
051200010330     C                   ENDSR
051300010330     C***
051400010330
051500010601
051600010601
051700010601
051800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051900010601     C     repfil        BEGSR
052000010601     C*
052100010601     C                   if        invfil = *zeros and
052200010601     C                             depfil > *zeros and
052300010629     C                             (vinflg = *blanks or
052400010629     C                              vinflg = *zeros)
052500010601     C
052600010601     C                   eval      invfil = depfil
052700010601     C                   endif
052800010601     C*
052900010601     C                   if        depfil <> invfil and
053000010601     C                             invfil > *zeros
053100010601     C                   eval      flgMulti = '1'
053200010601     C                   if        vinflg = *blanks
053300010601     C                   add       1             cntNonEl
053400010601     C                   endif
053500010601     C                   endif
053600010601     C*
053700010601     C                   if        vinflg = '2'
053800010601     C                   eval      flgStato = '2'
053900010601     C                   endif
054000010601     C*
054100010601     C                   ENDSR
054200010601     C***
054300010601
054400010601
054500010601
054600010330
054700010330
054800010330
054900990920      /TITLE Invio dei dati al punto operativo.
055000000613     C     invio         BEGSR
055100990920     C*
055200990920     C                   reset                   dscmz
055300010601     C                   move      invfil        cmzdst
055400990920     C                   eval      cmzfld = vlrfou
055500990920     C                   eval      cmzmbd = vlrhdl
055600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055700000710     C                   if        prmfir = *blanks
055800021014     C                   eval      cmzfla = 'FIVAB00F'
055900021014     C                   eval      cmzmba = 'FIVAB00F'
056000000710     C                   else
056100000710     C                   eval      cmzfla = prmfir
056200000710     C                   eval      cmzmba = prmfir
056300000710     C                   endif
056400990920     C                   eval      cmznrr = *zeros
056500990920     C                   move      §ctrok        cmznrr
056600021018     C                   eval      cmzlba = vlrfl1
056700990920     C                   call(e)   'TIS711C'
056800990920     C                   parm                    dscmz
056900990921     C                   parm      *blanks       esito
057000990920     C                   if        %error
057100990920     C                             or cmzerr = '1'
057200990921     C                             or esito  = '1'
057300000710     C                   eval      wrkesito = '3'
057400990920     C                   endif
057500990920     C*
057600000613     C                   ENDSR
057700990910
057800010601
057900010601
058000010601
058100010601
058200010601      /TITLE Invio dei dati al punto operativo.
058300010601     C     opeini        BEGSR
058400010601     C*
058500010601     C* Inizializzo flag e contatori operativi
058600010601     C                   movel     '0'           flgGiro           1
058700010601     C                   movel     '0'           flgMulti          1
058800010601     C                   movel     '1'           flgStato          1
058900010615     C                   movel     '0'           flgOk             1
059000010601     C                   z-add     *zeros        cntNonEl         10 0
059100010601     C                   z-add     *zeros        depfil            3 0
059200010601     C                   z-add     *zeros        invfil            3 0
059300010601     C*
059400010601     C                   ENDSR
059500010601     C***
059600010601
059700010601
059800010601
059900010330
060000010330
060100000613     C     *inzsr        BEGSR
060200990910     C*
060300990910     C     *entry        plist
060400990920     C                   parm                    tivlrds
060500990921     C                   parm      wrkesito      esito
060600000724     C                   parm                    prmlit
060700000710     C                   parm                    prmfir
060800010330     C*
060900010330     C* CALCOLA LA DATA CORRENTE
061000010330     C                   time                    wn14             14 0
061100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061200010330     C                   z-add     wn8           g08dat
061300010330     C                   z-add     *zeros        g08inv
061400010330     C                   movel     '0'           g08err
061500010330     C                   call      'XSRDA8'
061600010330     C                   parm                    wlbda8
061700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061800000613     C*
061900000613     C                   ENDSR
062000000613     C***
062100990908
062200040301     Otitva2p   E            riepilogo         2
062300990915     O                                              'Upload via Internet'
062400990915     O                                           +1 'Traduzione TIVIN00R -'
062500021014     O                                           +1 'FIVABWWR'
062600990915     O                                           +1 'Testate bolle'
062700990915     O          E            riepilogo   2
062800990915     O                       wrkdata
062900990915     O                       wrkora              +1
063000990915     O                       procname            +1
063100990915     O          E            riepilogo   2
063200990915     O                                              'Cliente..................:'
063300010605     O                       VABCCM        z     +1
063400990915     O          E            riepilogo   2
063500990920     O                                              'Riferimento Strategi.....:'
063600990920     O                       vlrhdl              +1
063700990915     O          E            riepilogo   2
063800990915     O                                              'Giusti...................:'
063900971022     O                       §CTROK        2   +  1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Sbagliati e corretti.....:'
064200971022     O                       §CTRMO        2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e scartati.....:'
064500971022     O                       §CTRNO        2   +  1
064600000613
064700040301     Otitva2ps  E            testdett          2
064800000613     O                                              'Upload via Internet'
064900000613     O                                           +1 'Traduzione TIVIN00R -'
065000021014     O                                           +1 'FIVABWWR'
065100000613     O                                           +1 'Testate bolle'
065200000616     O          E            testdett    3
065300000613     O                                           +2 'N° rec'
065400000613     O                                           +3 'Anteprima contenuto'
065500000616     O          E            rigadett    2
065600000613     O                       rrnum               +2
065700000621     O                       recko               +3
065800000616     O          E            findett     2
065900000613     O                       wrkdata
066000000613     O                       wrkora              +1
066100000613     O                       procname            +1
066200000616     O          E            findett     2
066300000613     O                                              'Cliente..................:'
066400010605     O                       VABCCM        z     +1
066500000616     O          E            findett     2
066600000613     O                                              'Riferimento Strategi.....:'
066700000613     O                       vlrhdl              +1
066800000616     O          E            findett     2
066900000613     O                                              'Giusti...................:'
067000000613     O                       §CTROK        2   +  1
067100000616     O          E            findett     2
067200000613     O                                              'Sbagliati e corretti.....:'
067300000613     O                       §CTRMO        2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e scartati.....:'
067600000613     O                       §CTRNO        2   +  1
067700000613** CMD - COMANDI CL
067800040301OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067900040301OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068000040301DLTOVR FILE(TITVA2P TITVA2PS) LVL(*)
068100010606
068200000613
