000100030702      /TITLE Upload via Internet: traduzione in FIVABWWR. (x DYVA - Cod. 0592173)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700030702     Ftitv60p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030702     Ftitv60ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600030702     D*------------
001700030702     D* SCHIERE D WRK
001800030702     D*------------
001900030702     D wSkSerie        S              1    DIM(27) CTDATA PERRCD(1)
002000030702     D wSkValori       S              2    DIM(27) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D wrkdata         s               d
003300010330     D wrkora          s               t
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500010330     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700030702     D i               s              2  0 INZ(*zeros)
003800030702     D wDDTSerie       s              1    INZ(*blanks)
003900030702     D wDDT            s              7    INZ(*zeros)
004000010330     D*------------------
004100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200010330     D*------------------
004300010330     D WLBDA8          DS                  INZ
004400010330     D  G08DAT                 1      8  0
004500010330     D  G08INV                 9     16  0
004600010330     D  G08ERR                17     17
004700010330     D  G08TGI                18     22  0
004800000313     D*
004900010330
005000010330
005100010330
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700010601     C*
005800010601     C                   exsr      opeini
005900010605     C                   exsr      rwvab
006000010601     C*
006100010601     C                   seton                                        lr
006200010601
006300010601
006400010601
006500010601
006600010601     C*--------------------------------------------------------
006700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006800010601     C*--------------------------------------------------------
006900010601     C     PREELA        BEGSR
007000010601     C*
007100010601     C* SE OCCORRE SPEDIRE IN FILIALE
007200010601     C                   if        invfil <> *zeros and
007300010601     C                             flgGiro = '0'
007400010601     C*
007500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007600010601     C                   eval      flgGiro = '1'
007700000724     C*
007800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008000010601     C     invfil        chain     azorg01l
008100000724     C                   if        %found
008200000724     C                   movel(p)  CMD(1)        depcmd
008300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008400000724     C*
008500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008600011113     C                   Z-ADD     150           LENGH            15 5
008700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000724     C                   PARM                    depcmd
008900000724     C                   PARM                    LENGH
009000000724     C*
009100000724     C                   endif
009200000616     C*
009300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009400000616     C                   movel(p)  CMD(2)        depcmd
009500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000613     C*
010300030702     C                   if        not %open(titv60ps)
010400030702     C                   open      titv60ps
010500000616     C                   except    testdett
010600000613     C                   endif
010700010601     C*
010800010601     C                   endif
010900010601     C*
011000010601     C                   ENDSR
011100010601     C***
011200010601
011300010601
011400010601
011500010601
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012100010601     C*--------------------------------------------------------
012200010601     C     ENDELA        BEGSR
012300990915     C*
012400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012500000613     C*
012600030702     C                   if        %open(titv60ps)
012700000616     C                   except    findett
012800030702     C                   close     titv60ps
012900000613     C                   endif
013000000616     C*
013100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    CMD(3)
013500000616     C                   PARM                    LENGH
013600000616     C*
013700010601     C                   ENDSR
013800010601     C***
013900010601
014000010601
014100010601
014200010601
014300000613
014400000613     C*--------------------------------------------------------
014500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014600000613     C*--------------------------------------------------------
014700000621     C     STPR          BEGSR
014800000613     C*
014900030702     C                   if        not %open(titv60p)
015000030702     C                   open      titv60p
015100990915     C                   endif
015200990915     C*
015300990915     C                   except    riepilogo
015400990915     C*
015500030702     C                   if        %open(titv60p)
015600030702     C                   close     titv60p
015700990914     C                   endif
015800000613     C*
015900000613     C                   ENDSR
016000000613     C***
016100000613
016200010330
016300010601
016400010601
016500010601
016600010330     C*--------------------------------------------------------
016700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016800010330     C*--------------------------------------------------------
016900010605     C     RWVAB         BEGSR
017000010330     C*
017100010330     C                   if        not %open(tivin00r)
017200010330     C                   open      tivin00r
017300010330     C                   endif
017400021025     C                   if        not %open(fivabwwr)
017500021025     C                   open      fivabwwr
017600010330     C                   endif
017700010330     C*
017800010604     C                   clear                   §CTROK
017900010604     C                   clear                   §CTRMO
018000010604     C                   clear                   §CTRNO
018100010330     C*
018200010330     C                   DO        *HIVAL
018300010330     C*
018400010330     C                   READ      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000010618     C*
019100010330     C                   if        vindta > *blanks
019200010330     C                   add       1             rrnum
019300010330     C*
019400010601     C                   if        *in70 = *off and
019500010330     C                             (vinflg = *blanks
019600010330     C                              or vinflg = '0'
019700010330     C                              or vinflg = '2')
019800010330     C*
019900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020000010711     C                   if        vinflg = *blanks or vinflg = '0'
020100010711     C                   clear                   vinmsg
020200010711     C                   endif
020300010601     C*
020400010330     C                   exsr      inzvar
020500010330     C                   exsr      defcam
020600010605     C                   exsr      impvab
020700010601     C*
020800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020900010605     C                   eval      depfil = VABLNP
021000010601     C                   exsr      repfil
021100010601     C                   if        depfil = invfil
021200021025     C                   if        vlrpoi = 999
021300021025     C                   MOVE(P)   invfil        VABFGS
021400021025     C                   else
021500021025     C                   MOVE(P)   vlrpoi        VABFGS
021600021025     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200010604     C  N31              ADD       1             §CTROK            7 0
022300010604     C   32              ADD       1             §CTRMO            7 0
022400010604     C   31              ADD       1             §CTRNO            7 0
022500021025     C  N31              WRITE     FIVAB000
022600010604     C*
022700010604     C                   if        *in31 = *off and
022800010604     C                             *in32 = *off
022900010604     C                   eval      vinflg = '1'
023000010604     C                   else
023100010604     C                   eval      recko = vindta
023200010604     C                   except    rigadett
023300010604     C                   eval      vinflg = '2'
023400010604     C                   endif
023500010604     C                   endif
023600010601     C*
023700010604     C                   endif
023800010604     C*
023900010330     C                   else
024000010330     C                   eval      vinflg = '1'
024100010330     C                   endif
024200010601     C*
024300010601     C  N70              update    tivin000
024400010330     C*
024500010330     C  N70              ENDdo
024600010601     C
024700010601     C*
024800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024900010601     C                   if        cntNonEl = *zeros or
025000010601     C                             flgMulti = '0'
025100010330     C* Se non ci sono record con errori ...
025200010601     C                   if        §ctrno = 0 and
025300010604     C                             §ctrmo = 0 and
025400010601     C                             flgStato <> '2'
025500010330     C* ... restituisco esito OK.
025600010330     C                   eval      wrkesito = '0'
025700010330     C                   else
025800010330     C                   if        §ctrok > 0
025900010330     C                   eval      wrkesito = '1'
026000010330     C                   else
026100010615     C                   if        flgOk = '0'
026200010615     C                   eval      wrkesito = '2'
026300010615     C                   else
026400010615     C                   eval      wrkesito = '6'
026500010615     C                   endif
026600010330     C                   endif
026700010330     C                   endif
026800010601     C                   else
026900010601     C                   eval      wrkesito = '9'
027000010601     C                   endif
027100010330     C*
027200010330     C                   if        %open(tivin00r)
027300010330     C                   close     tivin00r
027400010330     C                   endif
027500021025     C                   if        %open(fivabwwr)
027600021025     C                   close     fivabwwr
027700010330     C                   endif
027800010601     C*
027900010601     C                   if        vlrpoi <> 999
028000010601     C                   eval      invfil = vlrpoi
028100010601     C                   endif
028200010330     C*
028300010330     C                   if        §ctrok > 0
028400010601     C                             and invfil > *zeros
028500010330     C                   exsr      invio
028600010330     C                   endif
028700010601     C*
028800010618     C                   if        flgGiro = '1'
028900010601     C                   exsr      endela
029000010618     C                   endif
029100010330     C*
029200010330     C                   ENDSR
029300010330     C***
029400010601
029500010601
029600010601
029700010330     C*----------------------------------------------------*
029800010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029900010330     C*----------------------------------------------------*
030000010330     C     INZVAR        BEGSR
030100010330     C*
030200020204     C                   Z-ADD     *zeros        Num5_0            5 0
030300030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030400030606     C                   MOVEL     '0'           FlgCAS            1
030500010330     C*
030600010330     C                   ENDSR
030700010330     C*----------------------------------------------------*
030800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030900010330     C*----------------------------------------------------*
031000010330     C     DEFCAM        BEGSR
031100010330     C*
031200020204     C* Inizializzo il buffer del record da scrivere
031300021025     C                   CLEAR                   FIVAB000
031400020204     C* Imposto i valori di default...
031500030702     C                   EVAL      VABCCM = 0592173
031600030702     C                   EVAL      VABLNP = 059
031700030526     C                   EVAL      VABCTR = 000
031800031016     C                   EVAL      VABCTM = '7Q'
031900020204     C* ... e poi verifico se sono stati passati come parametri
032000020204     C                   IF        vlrppt > *blanks
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABCCM
032500020204     C                   ENDIF
032600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032700020204     C                   EXSR      CHKNUM
032800020204     C                   IF        PiInt=*on
032900020204     C                   Z-ADD     PiVal         VABLNP
033000020204     C                   ENDIF
033100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033200020204     C                   EXSR      CHKNUM
033300020204     C                   IF        PiInt=*on
033400020204     C                   Z-ADD     PiVal         VABCTR
033500020204     C                   ENDIF
033600020204     C                   ENDIF
033700020204     C*
033800010330     C                   ENDSR
033900010607     C*----------------------------------------------------*
034000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034100010607     C*----------------------------------------------------*
034200010607     C     IMPVAB        BEGSR
034300010607     C*
034400010607     C                   SETOFF                                       3132
034500030515     C*
034600010607     C                   MOVEL     datcor        VABAAS
034700010607     C                   MOVE      datcor        VABMGS
034800010607     C*
034900010607     C* Reperimento campi ALFA
035000010607     C*
035100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035200030702     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
035300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035500020117     C* ==
035600031029     C                   EVAL      VABRMO=%trim(%subst(vindta:357:5))
035700030702     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035800030606     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035900030606     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036000030606     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
036100070312     C                   EVAL      VABNT2=%trim(%subst(vindta:197:35))
036200070312     C                   EVAL      VABRD2='Tel. '+%trim(%subst(vindta:92:15))
036300030606     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
036400030702     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
036500030702     C                   IF        %subst(vindta:16:1) = '1'
036600030702     C                   EVAL      VABFFD='S'
036700030702     C                   ENDIF
036800010607     C*
036900010607     C* Reperimento campi NUMERICI
037000111219     C*
037100111219     C* CTR
037200111219     C                   IF        %subst(vindta:8:3) <> *blanks
037300111219     C                   EVAL      PiStr=%trim(%subst(vindta:8:3))
037400111219     C                   EXSR      CHKNUM
037500111219     C                   IF        PiInt=*on
037600111219     C                   z-add     PiVal         VABCTR
037700111219     C                   ELSE
037800111219     C                   SETON                                        32
037900111219     C                   EVAL      vinmsg = %trimr(vinmsg)
038000111219     C                             + ' ' + 'VABCTR'
038100111219     C                   ENDIF
038200111219     C                   ENDIF
038300020604     C*
038400030515     C* CAD
038500030606     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
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
039600030702     C* RMN/NSP
039700030702     C                   EVAL      wDDTSerie  = %subst(vindta:191:1)
039800030702     C                   Z-ADD     1             i
039900030702     C     wDDTSerie     LOOKUP    wSkSerie(i)                            55
040000030702     C                   IF        %found
040100030702     C                   EVAL      wDDT = wSkValori(i) +
040200030702     C                                    %trim(%subst(vindta:192:5))
040300030702     C                   ELSE
040400030702     C                   EVAL      wDDT = '00' +  %trim(%subst(vindta:192:5))
040500030702     C                   ENDIF
040600030702     C                   EVAL      PiStr=wDDT
040700011023     C                   EXSR      CHKNUM
040800011023     C                   IF        PiInt=*on
040900030225     C                   Z-ADD     PiVal         VABRMN
041000030225     C                   Z-ADD     PiVal         VABNSP
041100011023     C                   ELSE
041200020205     C                   EVAL      VABRMN = 1
041300011023     C                   ENDIF
041400030515     C* NCL
041500030606     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
041600010607     C                   EXSR      CHKNUM
041700010607     C                   IF        PiInt=*on
041800010607     C                   Z-ADD     PiVal         VABNCL
041900010607     C                   ELSE
042000010607     C                   SETON                                        32
042100010607     C                   Z-ADD     *zeros        VABNCL
042200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042300010607     C                             + ' ' + 'VABNCL'
042400010607     C                   ENDIF
042500030515     C* PKB
042600030606     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
042700031016     C                   EVAL      PiDecChr = '.'
042800010607     C                   EXSR      CHKNUM
042900010607     C                   IF        PiNum=*on
043000010607     C                   Z-ADD(H)  PiVal         VABPKB
043100010607     C                   ELSE
043200010607     C                   SETON                                        32
043300010607     C                   Z-ADD     *zeros        VABPKB
043400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043500010607     C                             + ' ' + 'VABPKB'
043600010607     C                   ENDIF
043700030702     C* CAS
043800030606     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
043900031016     C                   IF        PiStr <> '000000000.00'
044000030606     C                   MOVEL     '1'           FlgCAS
044100031016     C                   EVAL      PiDecChr = '.'
044200030606     C                   EXSR      CHKNUM
044300030606     C                   IF        PiNum=*on
044400030606     C                   Z-ADD     PiVal         VABCAS
044500080902     C                   EVAL      VABTIC = 'S2'
044600080902     C*                  IF        VABCAS >= 1000
044700080902     C*                  EVAL      VABTIC = 'CM'
044800080902     C*                  ELSE
044900080902     C*                  EVAL      VABTIC = *blanks
045000080902     C*                  ENDIF
045100030606     C                   ELSE
045200030606     C                   SETON                                        32
045300030606     C                   Z-ADD     *zeros        VABCAS
045400030606     C                   EVAL      vinmsg = %trimr(vinmsg)
045500030606     C                             + ' ' + 'VABCAS'
045600030606     C                   ENDIF
045700030606     C                   ENDIF
045800030606     C* IAS
045900030606     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
046000031016     C                   EVAL      PiDecChr = '.'
046100030606     C                   EXSR      CHKNUM
046200030606     C                   IF        PiNum=*on
046300030606     C                   Z-ADD     PiVal         VABIAS
046400030606     C                   ELSE
046500030606     C                   SETON                                        32
046600030606     C                   Z-ADD     *zeros        VABIAS
046700030606     C                   EVAL      vinmsg = %trimr(vinmsg)
046800030606     C                             + ' ' + 'VABIAS'
046900030606     C                   ENDIF
047000030606     C*
047100030606     C* Considerazioni finali su CBO/CAS
047200030606     C                   IF        FlgCAS = '1'
047300030606     C                   EVAL      VABCBO = '4'
047400030606     C                   ELSE
047500030606     C                   EVAL      VABCBO = '1'
047600030606     C                   ENDIF
047700020204     C*
047800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047900020204     C                   EXSR      CHKIMPDIV
048000020204     C*
048100010607     C                   ENDSR
048200010607     C*----------------------------------------------------*
048300010601
048400020204
048500020204     C*----------------------------------------------------*
048600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048700020204     C*----------------------------------------------------*
048800020204     C     CHKIMPDIV     BEGSR
048900020204     C*
049000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049100020204     C                   Z-ADD     *zeros        wrkDec            9 9
049200020204     C*
049300020204     C* Come prima cosa effettuo considerazioni sulla divisa
049400020204     C                   IF        vabIAS > *zeros
049500020204     C                   IF        vabVAS <> 'EUR'
049600020204     C                   EVAL      vabVAS =  'ITL'
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C                   IF        vabCAS > *zeros
050100020204     C                   IF        vabVCA <> 'EUR'
050200020204     C                   EVAL      vabVCA =  'ITL'
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C                   IF        vabVMD > *zeros
050700020204     C                   IF        vabVAD <> 'EUR'
050800020204     C                   EVAL      vabVAD =  'ITL'
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051300020204     C                   Z-ADD     vabIAS        wrkDec
051400020204     C                   IF        wrkDec > *zeros
051500020204     C                   IF        vabVAS = 'ITL'
051600020204     C                   EVAL      vabIAS = *zeros
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052100020204     C                   Z-ADD     vabCAS        wrkDec
052200020204     C                   IF        wrkDec > *zeros
052300020204     C                   IF        vabVCA = 'ITL'
052400020204     C                   EVAL      vabCAS = *zeros
052500020204     C                   ENDIF
052600020204     C                   ENDIF
052700020204     C*
052800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052900020204     C                   Z-ADD     vabVMD        wrkDec
053000020204     C                   IF        wrkDec > *zeros
053100020204     C                   IF        vabVAD = 'ITL'
053200020204     C                   EVAL      vabVMD = *zeros
053300020204     C                   ENDIF
053400020204     C                   ENDIF
053500020204     C*
053600020204     C                   ENDSR
053700020204     C***
053800020204
053900010330
054000010330
054100010330     C*----------------------------------------------------*
054200010330     C*  CONTROLLO NUMERICITA' CAMPI
054300010330     C*----------------------------------------------------*
054400010330     C     CHKNUM        BEGSR
054500010330     C*
054600010606     C                   IF        PiDecChr = *blanks
054700031016     C                   EVAL      PiDecChr = '.'
054800010606     C                   ENDIF
054900010606     C*
055000010606     C                   CALL(e)   'ISNUMERIC'
055100010330     C                   PARM                    PiStr            30
055200010606     C                   PARM                    PiDecChr          1
055300010330     C                   PARM      *ZEROS        PiVal            30 9
055400010330     C                   PARM      '0'           PiInt             1
055500010330     C                   PARM      '0'           PiNum             1
055600010330     C                   IF        %error
055700010606     C                   EVAL      PiNum=*off
055800010330     C                   ENDIF
055900010330     C*
056000010330     C                   ENDSR
056100010330     C***
056200010330
056300010601
056400010601
056500010601
056600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056700010601     C     repfil        BEGSR
056800010601     C*
056900010601     C                   if        invfil = *zeros and
057000010601     C                             depfil > *zeros and
057100010629     C                             (vinflg = *blanks or
057200010629     C                              vinflg = *zeros)
057300010601     C
057400010601     C                   eval      invfil = depfil
057500010601     C                   endif
057600010601     C*
057700010601     C                   if        depfil <> invfil and
057800010601     C                             invfil > *zeros
057900010601     C                   eval      flgMulti = '1'
058000010601     C                   if        vinflg = *blanks
058100010601     C                   add       1             cntNonEl
058200010601     C                   endif
058300010601     C                   endif
058400010601     C*
058500010601     C                   if        vinflg = '2'
058600010601     C                   eval      flgStato = '2'
058700010601     C                   endif
058800010601     C*
058900010601     C                   ENDSR
059000010601     C***
059100010601
059200010601
059300010601
059400010330
059500010330
059600010330
059700990920      /TITLE Invio dei dati al punto operativo.
059800000613     C     invio         BEGSR
059900990920     C*
060000990920     C                   reset                   dscmz
060100010601     C                   move      invfil        cmzdst
060200990920     C                   eval      cmzfld = vlrfou
060300990920     C                   eval      cmzmbd = vlrhdl
060400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500000710     C                   if        prmfir = *blanks
060600021025     C                   eval      cmzfla = 'FIVAB00F'
060700021025     C                   eval      cmzmba = 'FIVAB00F'
060800000710     C                   else
060900000710     C                   eval      cmzfla = prmfir
061000000710     C                   eval      cmzmba = prmfir
061100000710     C                   endif
061200990920     C                   eval      cmznrr = *zeros
061300990920     C                   move      §ctrok        cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500990920     C                   call(e)   'TIS711C'
061600990920     C                   parm                    dscmz
061700990921     C                   parm      *blanks       esito
061800990920     C                   if        %error
061900990920     C                             or cmzerr = '1'
062000990921     C                             or esito  = '1'
062100000710     C                   eval      wrkesito = '3'
062200990920     C                   endif
062300990920     C*
062400000613     C                   ENDSR
062500990910
062600010601
062700010601
062800010601
062900010601
063000010601      /TITLE Invio dei dati al punto operativo.
063100010601     C     opeini        BEGSR
063200010601     C*
063300010601     C* Inizializzo flag e contatori operativi
063400010601     C                   movel     '0'           flgGiro           1
063500010601     C                   movel     '0'           flgMulti          1
063600010601     C                   movel     '1'           flgStato          1
063700010615     C                   movel     '0'           flgOk             1
063800010601     C                   z-add     *zeros        cntNonEl         10 0
063900010601     C                   z-add     *zeros        depfil            3 0
064000010601     C                   z-add     *zeros        invfil            3 0
064100010601     C*
064200010601     C                   ENDSR
064300010601     C***
064400010601
064500010601
064600010601
064700010330
064800010330
064900000613     C     *inzsr        BEGSR
065000990910     C*
065100990910     C     *entry        plist
065200990920     C                   parm                    tivlrds
065300990921     C                   parm      wrkesito      esito
065400000724     C                   parm                    prmlit
065500000710     C                   parm                    prmfir
065600010330     C*
065700010330     C* CALCOLA LA DATA CORRENTE
065800010330     C                   time                    wn14             14 0
065900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066000010330     C                   z-add     wn8           g08dat
066100010330     C                   z-add     *zeros        g08inv
066200010330     C                   movel     '0'           g08err
066300010330     C                   call      'XSRDA8'
066400010330     C                   parm                    wlbda8
066500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066600000613     C*
066700000613     C                   ENDSR
066800000613     C***
066900990908
067000030702     Otitv60p   E            riepilogo         2
067100990915     O                                              'Upload via Internet'
067200990915     O                                           +1 'Traduzione TIVIN00R -'
067300021025     O                                           +1 'FIVABWWR'
067400990915     O                                           +1 'Testate bolle'
067500990915     O          E            riepilogo   2
067600990915     O                       wrkdata
067700990915     O                       wrkora              +1
067800990915     O                       procname            +1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Cliente..................:'
068100010605     O                       VABCCM        z     +1
068200990915     O          E            riepilogo   2
068300990920     O                                              'Riferimento Strategi.....:'
068400990920     O                       vlrhdl              +1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Giusti...................:'
068700971022     O                       §CTROK        2   +  1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Sbagliati e corretti.....:'
069000971022     O                       §CTRMO        2   +  1
069100990915     O          E            riepilogo   2
069200990915     O                                              'Sbagliati e scartati.....:'
069300971022     O                       §CTRNO        2   +  1
069400000613
069500030702     Otitv60ps  E            testdett          2
069600000613     O                                              'Upload via Internet'
069700000613     O                                           +1 'Traduzione TIVIN00R -'
069800021025     O                                           +1 'FIVABWWR'
069900000613     O                                           +1 'Testate bolle'
070000000616     O          E            testdett    3
070100000613     O                                           +2 'N° rec'
070200000613     O                                           +3 'Anteprima contenuto'
070300000616     O          E            rigadett    2
070400000613     O                       rrnum               +2
070500000621     O                       recko               +3
070600000616     O          E            findett     2
070700000613     O                       wrkdata
070800000613     O                       wrkora              +1
070900000613     O                       procname            +1
071000000616     O          E            findett     2
071100000613     O                                              'Cliente..................:'
071200010605     O                       VABCCM        z     +1
071300000616     O          E            findett     2
071400000613     O                                              'Riferimento Strategi.....:'
071500000613     O                       vlrhdl              +1
071600000616     O          E            findett     2
071700000613     O                                              'Giusti...................:'
071800000613     O                       §CTROK        2   +  1
071900000616     O          E            findett     2
072000000613     O                                              'Sbagliati e corretti.....:'
072100000613     O                       §CTRMO        2   +  1
072200000616     O          E            findett     2
072300000613     O                                              'Sbagliati e scartati.....:'
072400000613     O                       §CTRNO        2   +  1
072500000613** CMD - COMANDI CL
072600030702OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV60P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
072700030702OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV60PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
072800030702DLTOVR FILE(TITV60P TITV60PS) LVL(*)
072900010606
073000000613
073100030702** wSkSerie - Valori alfabetici delle serie utilizzate dal cliente x il numero DDT
073200030702
073300030702A
073400030702B
073500030702C
073600030702D
073700030702E
073800030702F
073900030702G
074000030702H
074100030702I
074200030702J
074300030702K
074400030702L
074500030702M
074600030702N
074700030702O
074800030702P
074900030702Q
075000030702R
075100030702S
075200030702T
075300030702U
075400030702V
075500030702W
075600030702X
075700030702Y
075800030702Z
075900030702** wSkValori - Valori numerici corrispondenti ai sopra elencati valori alfabetici
07600003070200
07610003070201
07620003070202
07630003070203
07640003070204
07650003070205
07660003070206
07670003070207
07680003070208
07690003070209
07700003070210
07710003070211
07720003070212
07730003070213
07740003070214
07750003070215
07760003070216
07770003070217
07780003070218
07790003070219
07800003070220
07810003070221
07820003070222
07830003070223
07840003070224
07850003070225
07860003070226
