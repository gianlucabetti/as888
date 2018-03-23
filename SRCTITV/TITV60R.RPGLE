000100030702      /TITLE Upload via Internet: traduzione in FIVABWWR. (x DYVA - Cod. 0592173)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700030702     D*------------
000800030702     D* SCHIERE D WRK
000900030702     D*------------
001000030702     D wSkSerie        S              1    DIM(27) CTDATA PERRCD(1)
001100030702     D wSkValori       S              2    DIM(27) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900010330     D esito           s              1
002000010330     D prmlit          s             10
002100010330     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300010330     D rrnum           s              6  0 INZ(*zeros)
002400030702     D i               s              2  0 INZ(*zeros)
002500030702     D wDDTSerie       s              1    INZ(*blanks)
002600030702     D wDDT            s              7    INZ(*zeros)
002700010330     D*------------------
002800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900010330     D*------------------
003000010330     D WLBDA8          DS                  INZ
003100010330     D  G08DAT                 1      8  0
003200010330     D  G08INV                 9     16  0
003300010330     D  G08ERR                17     17
003400010330     D  G08TGI                18     22  0
003500000313     D*
003600010330
003700010330
003800010330
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200010601     C*
004300010601     C                   exsr      opeini
004400010605     C                   exsr      rwvab
004500010601     C*
004600010601     C                   seton                                        lr
004700010601
004800010601
004900010601
005000010601
005100010601     C*--------------------------------------------------------
005200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005300010601     C*--------------------------------------------------------
005400010601     C     PREELA        BEGSR
005500010601     C*
005600010601     C* SE OCCORRE SPEDIRE IN FILIALE
005700010601     C                   if        invfil <> *zeros and
005800010601     C                             flgGiro = '0'
005900010601     C*
006000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006100010601     C                   eval      flgGiro = '1'
006200010601     C*
006300010601     C                   endif
006400010601     C*
006500010601     C                   ENDSR
006600010601     C***
006700010601
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601
007400010601     C*--------------------------------------------------------
007500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007600010601     C*--------------------------------------------------------
007700010601     C     ENDELA        BEGSR
007800000616     C*
007900010601     C                   ENDSR
008000010601     C***
008100000613
008200010330
008300010601
008400010601
008500010601
008600010330     C*--------------------------------------------------------
008700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008800010330     C*--------------------------------------------------------
008900010605     C     RWVAB         BEGSR
009000010330     C*
009100010330     C                   if        not %open(tivin00r)
009200010330     C                   open      tivin00r
009300010330     C                   endif
009400021025     C                   if        not %open(fivabwwr)
009500021025     C                   open      fivabwwr
009600010330     C                   endif
009700010330     C*
009800010604     C                   clear                   §CTROK
009900010604     C                   clear                   §CTRMO
010000010604     C                   clear                   §CTRNO
010100010330     C*
010200010330     C                   DO        *HIVAL
010300010330     C*
010400010330     C                   READ      tivin00r                               70
010500010618     C*
010600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010700010618     C                   if        vinflg = '1'
010800010618     C                   eval      flgOk = '1'
010900010618     C                   endif
011000010618     C*
011100010330     C                   if        vindta > *blanks
011200010330     C                   add       1             rrnum
011300010330     C*
011400010601     C                   if        *in70 = *off and
011500010330     C                             (vinflg = *blanks
011600010330     C                              or vinflg = '0'
011700010330     C                              or vinflg = '2')
011800010330     C*
011900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012000010711     C                   if        vinflg = *blanks or vinflg = '0'
012100010711     C                   clear                   vinmsg
012200010711     C                   endif
012300010601     C*
012400010330     C                   exsr      inzvar
012500010330     C                   exsr      defcam
012600010605     C                   exsr      impvab
012700010601     C*
012800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012900010605     C                   eval      depfil = VABLNP
013000010601     C                   exsr      repfil
013100010601     C                   if        depfil = invfil
013200021025     C                   if        vlrpoi = 999
013300021025     C                   MOVE(P)   invfil        VABFGS
013400021025     C                   else
013500021025     C                   MOVE(P)   vlrpoi        VABFGS
013600021025     C                   endif
013700010601     C*
013800010601     C                   exsr      PREELA
013900010601     C*
014000010604     C* Ebbene...
014100010604     C*
014200010604     C  N31              ADD       1             §CTROK            7 0
014300010604     C   32              ADD       1             §CTRMO            7 0
014400010604     C   31              ADD       1             §CTRNO            7 0
014500021025     C  N31              WRITE     FIVAB000
014600010604     C*
014700010604     C                   if        *in31 = *off and
014800010604     C                             *in32 = *off
014900010604     C                   eval      vinflg = '1'
015000010604     C                   else
015100010604     C                   eval      vinflg = '2'
015200010604     C                   endif
015300010604     C                   endif
015400010601     C*
015500010604     C                   endif
015600010604     C*
015700010330     C                   else
015800010330     C                   eval      vinflg = '1'
015900010330     C                   endif
016000010601     C*
016100010601     C  N70              update    tivin000
016200010330     C*
016300010330     C  N70              ENDdo
016400010601     C
016500010601     C*
016600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016700010601     C                   if        cntNonEl = *zeros or
016800010601     C                             flgMulti = '0'
016900010330     C* Se non ci sono record con errori ...
017000010601     C                   if        §ctrno = 0 and
017100010604     C                             §ctrmo = 0 and
017200010601     C                             flgStato <> '2'
017300010330     C* ... restituisco esito OK.
017400010330     C                   eval      wrkesito = '0'
017500010330     C                   else
017600010330     C                   if        §ctrok > 0
017700010330     C                   eval      wrkesito = '1'
017800010330     C                   else
017900010615     C                   if        flgOk = '0'
018000010615     C                   eval      wrkesito = '2'
018100010615     C                   else
018200010615     C                   eval      wrkesito = '6'
018300010615     C                   endif
018400010330     C                   endif
018500010330     C                   endif
018600010601     C                   else
018700010601     C                   eval      wrkesito = '9'
018800010601     C                   endif
018900010330     C*
019000010330     C                   if        %open(tivin00r)
019100010330     C                   close     tivin00r
019200010330     C                   endif
019300021025     C                   if        %open(fivabwwr)
019400021025     C                   close     fivabwwr
019500010330     C                   endif
019600010601     C*
019700010601     C                   if        vlrpoi <> 999
019800010601     C                   eval      invfil = vlrpoi
019900010601     C                   endif
020000010330     C*
020100010330     C                   if        §ctrok > 0
020200010601     C                             and invfil > *zeros
020300010330     C                   exsr      invio
020400010330     C                   endif
020500010601     C*
020600010618     C                   if        flgGiro = '1'
020700010601     C                   exsr      endela
020800010618     C                   endif
020900010330     C*
021000010330     C                   ENDSR
021100010330     C***
021200010601
021300010601
021400010601
021500010330     C*----------------------------------------------------*
021600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021700010330     C*----------------------------------------------------*
021800010330     C     INZVAR        BEGSR
021900010330     C*
022000020204     C                   Z-ADD     *zeros        Num5_0            5 0
022100030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022200030606     C                   MOVEL     '0'           FlgCAS            1
022300010330     C*
022400010330     C                   ENDSR
022500010330     C*----------------------------------------------------*
022600010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022700010330     C*----------------------------------------------------*
022800010330     C     DEFCAM        BEGSR
022900010330     C*
023000020204     C* Inizializzo il buffer del record da scrivere
023100021025     C                   CLEAR                   FIVAB000
023200020204     C* Imposto i valori di default...
023300030702     C                   EVAL      VABCCM = 0592173
023400030702     C                   EVAL      VABLNP = 059
023500030526     C                   EVAL      VABCTR = 000
023600031016     C                   EVAL      VABCTM = '7Q'
023700020204     C* ... e poi verifico se sono stati passati come parametri
023800020204     C                   IF        vlrppt > *blanks
023900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024000020204     C                   EXSR      CHKNUM
024100020204     C                   IF        PiInt=*on
024200020204     C                   Z-ADD     PiVal         VABCCM
024300020204     C                   ENDIF
024400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024500020204     C                   EXSR      CHKNUM
024600020204     C                   IF        PiInt=*on
024700020204     C                   Z-ADD     PiVal         VABLNP
024800020204     C                   ENDIF
024900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025000020204     C                   EXSR      CHKNUM
025100020204     C                   IF        PiInt=*on
025200020204     C                   Z-ADD     PiVal         VABCTR
025300020204     C                   ENDIF
025400020204     C                   ENDIF
025500020204     C*
025600010330     C                   ENDSR
025700010607     C*----------------------------------------------------*
025800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025900010607     C*----------------------------------------------------*
026000010607     C     IMPVAB        BEGSR
026100010607     C*
026200010607     C                   SETOFF                                       3132
026300030515     C*
026400010607     C                   MOVEL     datcor        VABAAS
026500010607     C                   MOVE      datcor        VABMGS
026600010607     C*
026700010607     C* Reperimento campi ALFA
026800010607     C*
026900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027000030702     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
027100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027300020117     C* ==
027400031029     C                   EVAL      VABRMO=%trim(%subst(vindta:357:5))
027500030702     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
027600030606     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
027700030606     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
027800030606     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
027900070312     C                   EVAL      VABNT2=%trim(%subst(vindta:197:35))
028000070312     C                   EVAL      VABRD2='Tel. '+%trim(%subst(vindta:92:15))
028100030606     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
028200030702     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
028300030702     C                   IF        %subst(vindta:16:1) = '1'
028400030702     C                   EVAL      VABFFD='S'
028500030702     C                   ENDIF
028600010607     C*
028700010607     C* Reperimento campi NUMERICI
028800111219     C*
028900111219     C* CTR
029000111219     C                   IF        %subst(vindta:8:3) <> *blanks
029100111219     C                   EVAL      PiStr=%trim(%subst(vindta:8:3))
029200111219     C                   EXSR      CHKNUM
029300111219     C                   IF        PiInt=*on
029400111219     C                   z-add     PiVal         VABCTR
029500111219     C                   ELSE
029600111219     C                   SETON                                        32
029700111219     C                   EVAL      vinmsg = %trimr(vinmsg)
029800111219     C                             + ' ' + 'VABCTR'
029900111219     C                   ENDIF
030000111219     C                   ENDIF
030100020604     C*
030200030515     C* CAD
030300030606     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
030400010607     C                   EXSR      CHKNUM
030500010607     C                   IF        PiInt=*on
030600020204     C                   Z-ADD     PiVal         Num5_0
030700020204     C                   MOVEL(p)  Num5_0        VABCAD
030800010607     C                   ELSE
030900010607     C                   SETON                                        32
031000020204     C                   EVAL      VABCAD = *zeros
031100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031200020204     C                             + ' ' + 'VABCAD'
031300010607     C                   ENDIF
031400030702     C* RMN/NSP
031500030702     C                   EVAL      wDDTSerie  = %subst(vindta:191:1)
031600030702     C                   Z-ADD     1             i
031700030702     C     wDDTSerie     LOOKUP    wSkSerie(i)                            55
031800030702     C                   IF        %found
031900030702     C                   EVAL      wDDT = wSkValori(i) +
032000030702     C                                    %trim(%subst(vindta:192:5))
032100030702     C                   ELSE
032200030702     C                   EVAL      wDDT = '00' +  %trim(%subst(vindta:192:5))
032300030702     C                   ENDIF
032400030702     C                   EVAL      PiStr=wDDT
032500011023     C                   EXSR      CHKNUM
032600011023     C                   IF        PiInt=*on
032700030225     C                   Z-ADD     PiVal         VABRMN
032800030225     C                   Z-ADD     PiVal         VABNSP
032900011023     C                   ELSE
033000020205     C                   EVAL      VABRMN = 1
033100011023     C                   ENDIF
033200030515     C* NCL
033300030606     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
033400010607     C                   EXSR      CHKNUM
033500010607     C                   IF        PiInt=*on
033600010607     C                   Z-ADD     PiVal         VABNCL
033700010607     C                   ELSE
033800010607     C                   SETON                                        32
033900010607     C                   Z-ADD     *zeros        VABNCL
034000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034100010607     C                             + ' ' + 'VABNCL'
034200010607     C                   ENDIF
034300030515     C* PKB
034400030606     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
034500031016     C                   EVAL      PiDecChr = '.'
034600010607     C                   EXSR      CHKNUM
034700010607     C                   IF        PiNum=*on
034800010607     C                   Z-ADD(H)  PiVal         VABPKB
034900010607     C                   ELSE
035000010607     C                   SETON                                        32
035100010607     C                   Z-ADD     *zeros        VABPKB
035200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035300010607     C                             + ' ' + 'VABPKB'
035400010607     C                   ENDIF
035500030702     C* CAS
035600030606     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
035700031016     C                   IF        PiStr <> '000000000.00'
035800030606     C                   MOVEL     '1'           FlgCAS
035900031016     C                   EVAL      PiDecChr = '.'
036000030606     C                   EXSR      CHKNUM
036100030606     C                   IF        PiNum=*on
036200030606     C                   Z-ADD     PiVal         VABCAS
036300080902     C                   EVAL      VABTIC = 'S2'
036400080902     C*                  IF        VABCAS >= 1000
036500080902     C*                  EVAL      VABTIC = 'CM'
036600080902     C*                  ELSE
036700080902     C*                  EVAL      VABTIC = *blanks
036800080902     C*                  ENDIF
036900030606     C                   ELSE
037000030606     C                   SETON                                        32
037100030606     C                   Z-ADD     *zeros        VABCAS
037200030606     C                   EVAL      vinmsg = %trimr(vinmsg)
037300030606     C                             + ' ' + 'VABCAS'
037400030606     C                   ENDIF
037500030606     C                   ENDIF
037600030606     C* IAS
037700030606     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
037800031016     C                   EVAL      PiDecChr = '.'
037900030606     C                   EXSR      CHKNUM
038000030606     C                   IF        PiNum=*on
038100030606     C                   Z-ADD     PiVal         VABIAS
038200030606     C                   ELSE
038300030606     C                   SETON                                        32
038400030606     C                   Z-ADD     *zeros        VABIAS
038500030606     C                   EVAL      vinmsg = %trimr(vinmsg)
038600030606     C                             + ' ' + 'VABIAS'
038700030606     C                   ENDIF
038800030606     C*
038900030606     C* Considerazioni finali su CBO/CAS
039000030606     C                   IF        FlgCAS = '1'
039100030606     C                   EVAL      VABCBO = '4'
039200030606     C                   ELSE
039300030606     C                   EVAL      VABCBO = '1'
039400030606     C                   ENDIF
039500020204     C*
039600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039700020204     C                   EXSR      CHKIMPDIV
039800020204     C*
039900010607     C                   ENDSR
040000010607     C*----------------------------------------------------*
040100010601
040200020204
040300020204     C*----------------------------------------------------*
040400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040500020204     C*----------------------------------------------------*
040600020204     C     CHKIMPDIV     BEGSR
040700020204     C*
040800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040900020204     C                   Z-ADD     *zeros        wrkDec            9 9
041000020204     C*
041100020204     C* Come prima cosa effettuo considerazioni sulla divisa
041200020204     C                   IF        vabIAS > *zeros
041300020204     C                   IF        vabVAS <> 'EUR'
041400020204     C                   EVAL      vabVAS =  'ITL'
041500020204     C                   ENDIF
041600020204     C                   ENDIF
041700020204     C*
041800020204     C                   IF        vabCAS > *zeros
041900020204     C                   IF        vabVCA <> 'EUR'
042000020204     C                   EVAL      vabVCA =  'ITL'
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C                   IF        vabVMD > *zeros
042500020204     C                   IF        vabVAD <> 'EUR'
042600020204     C                   EVAL      vabVAD =  'ITL'
042700020204     C                   ENDIF
042800020204     C                   ENDIF
042900020204     C*
043000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043100020204     C                   Z-ADD     vabIAS        wrkDec
043200020204     C                   IF        wrkDec > *zeros
043300020204     C                   IF        vabVAS = 'ITL'
043400020204     C                   EVAL      vabIAS = *zeros
043500020204     C                   ENDIF
043600020204     C                   ENDIF
043700020204     C*
043800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043900020204     C                   Z-ADD     vabCAS        wrkDec
044000020204     C                   IF        wrkDec > *zeros
044100020204     C                   IF        vabVCA = 'ITL'
044200020204     C                   EVAL      vabCAS = *zeros
044300020204     C                   ENDIF
044400020204     C                   ENDIF
044500020204     C*
044600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044700020204     C                   Z-ADD     vabVMD        wrkDec
044800020204     C                   IF        wrkDec > *zeros
044900020204     C                   IF        vabVAD = 'ITL'
045000020204     C                   EVAL      vabVMD = *zeros
045100020204     C                   ENDIF
045200020204     C                   ENDIF
045300020204     C*
045400020204     C                   ENDSR
045500020204     C***
045600020204
045700010330
045800010330
045900010330     C*----------------------------------------------------*
046000010330     C*  CONTROLLO NUMERICITA' CAMPI
046100010330     C*----------------------------------------------------*
046200010330     C     CHKNUM        BEGSR
046300010330     C*
046400010606     C                   IF        PiDecChr = *blanks
046500031016     C                   EVAL      PiDecChr = '.'
046600010606     C                   ENDIF
046700010606     C*
046800010606     C                   CALL(e)   'ISNUMERIC'
046900010330     C                   PARM                    PiStr            30
047000010606     C                   PARM                    PiDecChr          1
047100010330     C                   PARM      *ZEROS        PiVal            30 9
047200010330     C                   PARM      '0'           PiInt             1
047300010330     C                   PARM      '0'           PiNum             1
047400010330     C                   IF        %error
047500010606     C                   EVAL      PiNum=*off
047600010330     C                   ENDIF
047700010330     C*
047800010330     C                   ENDSR
047900010330     C***
048000010330
048100010601
048200010601
048300010601
048400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048500010601     C     repfil        BEGSR
048600010601     C*
048700010601     C                   if        invfil = *zeros and
048800010601     C                             depfil > *zeros and
048900010629     C                             (vinflg = *blanks or
049000010629     C                              vinflg = *zeros)
049100010601     C
049200010601     C                   eval      invfil = depfil
049300010601     C                   endif
049400010601     C*
049500010601     C                   if        depfil <> invfil and
049600010601     C                             invfil > *zeros
049700010601     C                   eval      flgMulti = '1'
049800010601     C                   if        vinflg = *blanks
049900010601     C                   add       1             cntNonEl
050000010601     C                   endif
050100010601     C                   endif
050200010601     C*
050300010601     C                   if        vinflg = '2'
050400010601     C                   eval      flgStato = '2'
050500010601     C                   endif
050600010601     C*
050700010601     C                   ENDSR
050800010601     C***
050900010601
051000010601
051100010601
051200010330
051300010330
051400010330
051500990920      /TITLE Invio dei dati al punto operativo.
051600000613     C     invio         BEGSR
051700990920     C*
051800990920     C                   reset                   dscmz
051900010601     C                   move      invfil        cmzdst
052000990920     C                   eval      cmzfld = vlrfou
052100990920     C                   eval      cmzmbd = vlrhdl
052200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052300000710     C                   if        prmfir = *blanks
052400021025     C                   eval      cmzfla = 'FIVAB00F'
052500021025     C                   eval      cmzmba = 'FIVAB00F'
052600000710     C                   else
052700000710     C                   eval      cmzfla = prmfir
052800000710     C                   eval      cmzmba = prmfir
052900000710     C                   endif
053000990920     C                   eval      cmznrr = *zeros
053100990920     C                   move      §ctrok        cmznrr
053200021018     C                   eval      cmzlba = vlrfl1
053300990920     C                   call(e)   'TIS711C'
053400990920     C                   parm                    dscmz
053500990921     C                   parm      *blanks       esito
053600990920     C                   if        %error
053700990920     C                             or cmzerr = '1'
053800990921     C                             or esito  = '1'
053900000710     C                   eval      wrkesito = '3'
054000990920     C                   endif
054100990920     C*
054200000613     C                   ENDSR
054300990910
054400010601
054500010601
054600010601
054700010601
054800010601      /TITLE Invio dei dati al punto operativo.
054900010601     C     opeini        BEGSR
055000010601     C*
055100010601     C* Inizializzo flag e contatori operativi
055200010601     C                   movel     '0'           flgGiro           1
055300010601     C                   movel     '0'           flgMulti          1
055400010601     C                   movel     '1'           flgStato          1
055500010615     C                   movel     '0'           flgOk             1
055600010601     C                   z-add     *zeros        cntNonEl         10 0
055700010601     C                   z-add     *zeros        depfil            3 0
055800010601     C                   z-add     *zeros        invfil            3 0
055900010601     C*
056000010601     C                   ENDSR
056100010601     C***
056200010601
056300010601
056400010601
056500010330
056600010330
056700000613     C     *inzsr        BEGSR
056800990910     C*
056900990910     C     *entry        plist
057000990920     C                   parm                    tivlrds
057100990921     C                   parm      wrkesito      esito
057200000724     C                   parm                    prmlit
057300000710     C                   parm                    prmfir
057400010330     C*
057500010330     C* CALCOLA LA DATA CORRENTE
057600010330     C                   time                    wn14             14 0
057700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057800010330     C                   z-add     wn8           g08dat
057900010330     C                   z-add     *zeros        g08inv
058000010330     C                   movel     '0'           g08err
058100010330     C                   call      'XSRDA8'
058200010330     C                   parm                    wlbda8
058300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058400000613     C*
058500000613     C                   ENDSR
058600000613     C***
058700030702** wSkSerie - Valori alfabetici delle serie utilizzate dal cliente x il numero DDT
058800030702
058900030702A
059000030702B
059100030702C
059200030702D
059300030702E
059400030702F
059500030702G
059600030702H
059700030702I
059800030702J
059900030702K
060000030702L
060100030702M
060200030702N
060300030702O
060400030702P
060500030702Q
060600030702R
060700030702S
060800030702T
060900030702U
061000030702V
061100030702W
061200030702X
061300030702Y
061400030702Z
061500030702** wSkValori - Valori numerici corrispondenti ai sopra elencati valori alfabetici
06160003070200
06170003070201
06180003070202
06190003070203
06200003070204
06210003070205
06220003070206
06230003070207
06240003070208
06250003070209
06260003070210
06270003070211
06280003070212
06290003070213
06300003070214
06310003070215
06320003070216
06330003070217
06340003070218
06350003070219
06360003070220
06370003070221
06380003070222
06390003070223
06400003070224
06410003070225
06420003070226
