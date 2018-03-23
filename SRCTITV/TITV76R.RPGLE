000100031124      /TITLE Upload via Internet: traduzione in FIVABWWR. (x AUTOTRASP. SALTI - Cod. 0930266)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900010330     D*------------------
002000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100010330     D*------------------
002200010330     D WLBDA8          DS                  INZ
002300010330     D  G08DAT                 1      8  0
002400010330     D  G08INV                 9     16  0
002500010330     D  G08ERR                17     17
002600010330     D  G08TGI                18     22  0
002700000313     D*
002800010330
002900010330
003000010330
003100000913     C                   reset                   rrnum
003200990921     C                   reset                   esito
003300990921     C                   reset                   wrkesito
003400010601     C*
003500010601     C                   exsr      opeini
003600010605     C                   exsr      rwvab
003700010601     C*
003800010601     C                   seton                                        lr
003900010601
004000010601
004100010601
004200010601
004300010601     C*--------------------------------------------------------
004400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004500010601     C*--------------------------------------------------------
004600010601     C     PREELA        BEGSR
004700010601     C*
004800010601     C* SE OCCORRE SPEDIRE IN FILIALE
004900010601     C                   if        invfil <> *zeros and
005000010601     C                             flgGiro = '0'
005100010601     C*
005200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005300010601     C                   eval      flgGiro = '1'
005400010601     C*
005500010601     C                   endif
005600010601     C*
005700010601     C                   ENDSR
005800010601     C***
005900010601
006000010601
006100010601
006200010601
006300010601
006400010601
006500010601
006600010601     C*--------------------------------------------------------
006700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006800010601     C*--------------------------------------------------------
006900010601     C     ENDELA        BEGSR
007000000616     C*
007100010601     C                   ENDSR
007200010601     C***
007300000613
007400010330
007500010601
007600010601
007700010601
007800010330     C*--------------------------------------------------------
007900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008000010330     C*--------------------------------------------------------
008100010605     C     RWVAB         BEGSR
008200010330     C*
008300010330     C                   if        not %open(tivin00r)
008400010330     C                   open      tivin00r
008500010330     C                   endif
008600021025     C                   if        not %open(fivabwwr)
008700021025     C                   open      fivabwwr
008800010330     C                   endif
008900010330     C*
009000010604     C                   clear                   §CTROK
009100010604     C                   clear                   §CTRMO
009200010604     C                   clear                   §CTRNO
009300010330     C*
009400010330     C                   DO        *HIVAL
009500010330     C*
009600010330     C                   READ      tivin00r                               70
009700010618     C*
009800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009900010618     C                   if        vinflg = '1'
010000010618     C                   eval      flgOk = '1'
010100010618     C                   endif
010200010618     C*
010300010330     C                   if        vindta > *blanks
010400010330     C                   add       1             rrnum
010500010330     C*
010600010601     C                   if        *in70 = *off and
010700010330     C                             (vinflg = *blanks
010800010330     C                              or vinflg = '0'
010900010330     C                              or vinflg = '2')
011000010330     C*
011100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011200010711     C                   if        vinflg = *blanks or vinflg = '0'
011300010711     C                   clear                   vinmsg
011400010711     C                   endif
011500010601     C*
011600010330     C                   exsr      inzvar
011700010330     C                   exsr      defcam
011800010605     C                   exsr      impvab
011900010601     C*
012000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012100010605     C                   eval      depfil = VABLNP
012200010601     C                   exsr      repfil
012300010601     C                   if        depfil = invfil
012400021025     C                   if        vlrpoi = 999
012500021025     C                   MOVE(P)   invfil        VABFGS
012600021025     C                   else
012700021025     C                   MOVE(P)   vlrpoi        VABFGS
012800021025     C                   endif
012900010601     C*
013000010601     C                   exsr      PREELA
013100010601     C*
013200010604     C* Ebbene...
013300010604     C*
013400010604     C  N31              ADD       1             §CTROK            7 0
013500010604     C   32              ADD       1             §CTRMO            7 0
013600010604     C   31              ADD       1             §CTRNO            7 0
013700021025     C  N31              WRITE     FIVAB000
013800010604     C*
013900010604     C                   if        *in31 = *off and
014000010604     C                             *in32 = *off
014100010604     C                   eval      vinflg = '1'
014200010604     C                   else
014300010604     C                   eval      vinflg = '2'
014400010604     C                   endif
014500010604     C                   endif
014600010601     C*
014700010604     C                   endif
014800010604     C*
014900010330     C                   else
015000010330     C                   eval      vinflg = '1'
015100010330     C                   endif
015200010601     C*
015300010601     C  N70              update    tivin000
015400010330     C*
015500010330     C  N70              ENDdo
015600010601     C
015700010601     C*
015800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015900010601     C                   if        cntNonEl = *zeros or
016000010601     C                             flgMulti = '0'
016100010330     C* Se non ci sono record con errori ...
016200010601     C                   if        §ctrno = 0 and
016300010604     C                             §ctrmo = 0 and
016400010601     C                             flgStato <> '2'
016500010330     C* ... restituisco esito OK.
016600010330     C                   eval      wrkesito = '0'
016700010330     C                   else
016800010330     C                   if        §ctrok > 0
016900010330     C                   eval      wrkesito = '1'
017000010330     C                   else
017100010615     C                   if        flgOk = '0'
017200010615     C                   eval      wrkesito = '2'
017300010615     C                   else
017400010615     C                   eval      wrkesito = '6'
017500010615     C                   endif
017600010330     C                   endif
017700010330     C                   endif
017800010601     C                   else
017900010601     C                   eval      wrkesito = '9'
018000010601     C                   endif
018100010330     C*
018200010330     C                   if        %open(tivin00r)
018300010330     C                   close     tivin00r
018400010330     C                   endif
018500021025     C                   if        %open(fivabwwr)
018600021025     C                   close     fivabwwr
018700010330     C                   endif
018800010601     C*
018900010601     C                   if        vlrpoi <> 999
019000010601     C                   eval      invfil = vlrpoi
019100010601     C                   endif
019200010330     C*
019300010330     C                   if        §ctrok > 0
019400010601     C                             and invfil > *zeros
019500010330     C                   exsr      invio
019600010330     C                   endif
019700010601     C*
019800010618     C                   if        flgGiro = '1'
019900010601     C                   exsr      endela
020000010618     C                   endif
020100010330     C*
020200010330     C                   ENDSR
020300010330     C***
020400010601
020500010601
020600010601
020700010330     C*----------------------------------------------------*
020800010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900010330     C*----------------------------------------------------*
021000010330     C     INZVAR        BEGSR
021100010330     C*
021200020204     C                   Z-ADD     *zeros        Num5_0            5 0
021300030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021400030606     C                   MOVEL     '0'           FlgCAS            1
021500010330     C*
021600010330     C                   ENDSR
021700010330     C*----------------------------------------------------*
021800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
021900010330     C*----------------------------------------------------*
022000010330     C     DEFCAM        BEGSR
022100010330     C*
022200020204     C* Inizializzo il buffer del record da scrivere
022300021025     C                   CLEAR                   FIVAB000
022400020204     C* Imposto i valori di default...
022500031124     C                   EVAL      VABCCM = 0930266
022600031124     C                   EVAL      VABLNP = 093
022700031013     C                   EVAL      VABCTR = 000
022800031124     C                   EVAL      VABCTM = '7K'
022900070330     C                   EVAL      VABTSP = 'C'
023000070330     C                   EVAL      VABCBO = '1'
023100020204     C* ... e poi verifico se sono stati passati come parametri
023200020204     C                   IF        vlrppt > *blanks
023300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020204     C                   EXSR      CHKNUM
023500020204     C                   IF        PiInt=*on
023600020204     C                   Z-ADD     PiVal         VABCCM
023700020204     C                   ENDIF
023800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023900020204     C                   EXSR      CHKNUM
024000020204     C                   IF        PiInt=*on
024100020204     C                   Z-ADD     PiVal         VABLNP
024200020204     C                   ENDIF
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020204     C                   Z-ADD     PiVal         VABCTR
024700020204     C                   ENDIF
024800020204     C                   ENDIF
024900020204     C*
025000010330     C                   ENDSR
025100010607     C*----------------------------------------------------*
025200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025300010607     C*----------------------------------------------------*
025400010607     C     IMPVAB        BEGSR
025500010607     C*
025600010607     C                   SETOFF                                       3132
025700030515     C*
025800010607     C                   MOVEL     datcor        VABAAS
025900010607     C                   MOVE      datcor        VABMGS
026000010607     C*
026100010607     C* Reperimento campi ALFA
026200010607     C*
026300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026400070330     C                   EVAL      VABRSD=%trim(%subst(vindta:243:30))
026500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026700020117     C* ==
026800070330     C                   EVAL      VABRMO=%trim(%subst(vindta:53:25))
026900070330     C                   EVAL      VABIND=%trim(%subst(vindta:273:30))
027000070330     C                   EVAL      VABLOD=%trim(%subst(vindta:303:30))
027100070330     C                   EVAL      VABPRD=%trim(%subst(vindta:333:2))
027200070330     C                   EVAL      VABRMA=%trim(%subst(vindta:7:7))
027300070330     C                   EVAL      VABNOT=%trim(%subst(vindta:550:30))
027400070330     C                   EVAL      VABNT2=%trim(%subst(vindta:580:30))
027500070424     C                   IF        %trim(%subst(vindta:533:1)) = 'T'
027600070424     C                   EVAL      VABTCR='P'
027700070424     C                   ENDIF
027800070424     C                   IF        %trim(%subst(vindta:612:1)) = '1'
027900070424     C                   EVAL      VABTC1='A'
028000070424     C                   ENDIF
028100031124     C*
028200031124     C* Effettuo considerazioin sulla nazione destinatario
028300070424     C                   IF        %trim(%subst(vindta:340:3)) = '   '          * ITALIA
028400070424     C                   EVAL      VABNZD = *blanks
028500070424     C                   ENDIF
028600070424     C                   IF        %trim(%subst(vindta:340:3)) = 'UST'          * AUSTRIA
028700070424     C                   EVAL      VABNZD = 'A'
028800070424     C                   ENDIF
028900070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ANI'          * DANIMARCA
029000070424     C                   EVAL      VABNZD = 'DK'
029100070424     C                   ENDIF
029200070424     C                   IF        %trim(%subst(vindta:340:3)) = 'NGH'          * GRAN BRETAGNA
029300070424     C                   EVAL      VABNZD = 'GB'
029400070424     C                   ENDIF
029500070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ELG'          * BELGIO
029600070424     C                   EVAL      VABNZD = 'B'
029700070424     C                   ENDIF
029800070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ERM'          * GERMANIA
029900070424     C                   EVAL      VABNZD = 'D'
030000070424     C                   ENDIF
030100070424     C                   IF        %trim(%subst(vindta:340:3)) = 'PAG'          * SPAGNA
030200070424     C                   EVAL      VABNZD = 'E'
030300070424     C                   ENDIF
030400070424     C                   IF        %trim(%subst(vindta:340:3)) = 'RAN'          * FRANCIA
030500070424     C                   EVAL      VABNZD = 'F'
030600070424     C                   ENDIF
030700070424     C                   IF        %trim(%subst(vindta:340:3)) = 'USS'          * LUSSEMBURGO
030800070424     C                   EVAL      VABNZD = 'L'
030900070424     C                   ENDIF
031000070424     C                   IF        %trim(%subst(vindta:340:3)) = 'ORT'          * PORTOGALLO
031100070424     C                   EVAL      VABNZD = 'P'
031200070424     C                   ENDIF
031300070424     C                   IF        %trim(%subst(vindta:340:3)) = 'LAN'          * OLANDA
031400070424     C                   EVAL      VABNZD = 'NL'
031500070424     C                   ENDIF
031600070424     C                   IF        %trim(%subst(vindta:340:3)) = 'Z'            * REP. CECA
031700070424     C                   EVAL      VABNZD = 'CZ'
031800070424     C                   ENDIF
031900070424     C                   IF        %trim(%subst(vindta:340:3)) = 'VEZ'          * SVEZIA
032000070424     C                   EVAL      VABNZD = 'S'
032100070424     C                   ENDIF
032200070424     C                   IF        %trim(%subst(vindta:340:3)) = 'VIZ'          * SVIZZERA
032300070424     C                   EVAL      VABNZD = 'CH'
032400070424     C                   ENDIF
032500070424     C*
032600031124     C* In relazione alla nazione destinatario modifico la tariffa
032700070424     C                   IF        VABNZD <> *blanks
032800070424     C                   EVAL      VABCTR = 001
032900070424     C                   ENDIF
033000010607     C*
033100010607     C* Reperimento campi NUMERICI
033200020604     C*
033300070424     C* DCR
033400070424     C                   IF        %subst(vindta:517:8) <> *all'0'
033500070424     C                   EVAL      PiStr=%trim(%subst(vindta:517:8))
033600070424     C                   EXSR      CHKNUM
033700070424     C                   IF        PiInt=*on
033800070424     C                   Z-ADD     PiVal         VABDCR
033900070424     C                   ELSE
034000070424     C                   SETON                                        32
034100070424     C                   EVAL      VABDCR = 1
034200070424     C                   EVAL      vinmsg = %trimr(vinmsg)
034300070424     C                             + ' ' + 'VABDCR'
034400070424     C                   ENDIF
034500070424     C                   ENDIF
034600031029     C* CAD
034700070330     C                   EVAL      PiStr=%trim(%subst(vindta:343:10))
034800010607     C                   EXSR      CHKNUM
034900010607     C                   IF        PiInt=*on
035000031124     C                   Z-ADD     PiVal         Num5_0
035100031124     C                   MOVEL(p)  Num5_0        VABCAD
035200010607     C                   ELSE
035300010607     C                   SETON                                        32
035400020204     C                   EVAL      VABCAD = *zeros
035500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035600020204     C                             + ' ' + 'VABCAD'
035700010607     C                   ENDIF
035800030515     C* RMN/NSP
035900070330     C                   EVAL      PiStr=%trim(%subst(vindta:163:15))
036000011023     C                   EXSR      CHKNUM
036100011023     C                   IF        PiInt=*on
036200030225     C                   Z-ADD     PiVal         VABRMN
036300031013     C                   MOVE(P)   VABRMN        VABNSP
036400011023     C                   ELSE
036500020205     C                   EVAL      VABRMN = 1
036600011023     C                   ENDIF
036700030515     C* NCL
036800070330     C                   EVAL      PiStr=%trim(%subst(vindta:373:5))
036900010607     C                   EXSR      CHKNUM
037000010607     C                   IF        PiInt=*on
037100010607     C                   Z-ADD     PiVal         VABNCL
037200010607     C                   ELSE
037300010607     C                   SETON                                        32
037400010607     C                   Z-ADD     *zeros        VABNCL
037500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037600010607     C                             + ' ' + 'VABNCL'
037700010607     C                   ENDIF
037800031013     C* PKB
037900070330     C                   EVAL      PiStr=%trim(%subst(vindta:378:8))
038000031013     C                   EXSR      CHKNUM
038100031017     C                   IF        PiNum=*on
038200070330     C                   EVAL      VABPKB = PiVal/1000                          * gestisco 3 decim.
038300031013     C                   ELSE
038400031013     C                   SETON                                        32
038500031013     C                   Z-ADD     *zeros        VABPKB
038600031013     C                   EVAL      vinmsg = %trimr(vinmsg)
038700031013     C                             + ' ' + 'VABPKB'
038800031013     C                   ENDIF
038900031124     C* CAS
039000070330     C                   IF        %subst(vindta:413:13) <> '0000000000000'
039100031124     C                   EVAL      FlgCAS = '1'
039200070330     C                   EVAL      VABVCA=%trim(%subst(vindta:426:3))
039300070330     C*
039400070330     C                   IF        %trim(%subst(vindta:623:3))='001'
039500070330     C                   EVAL      VABTIC=*blanks
039600070330     C                   ENDIF
039700070330     C                   IF        %trim(%subst(vindta:623:3))='002'
039800070330     C                   EVAL      VABTIC='OM'
039900070330     C                   ENDIF
040000070330     C                   IF        %trim(%subst(vindta:623:3))='003'
040100070330     C                   EVAL      VABTIC='CM'
040200070330     C                   ENDIF
040300070330     C                   IF        %trim(%subst(vindta:623:3))='004'
040400070330     C                   EVAL      VABTIC='BM'
040500070330     C                   ENDIF
040600070330     C*
040700070330     C                   EVAL      PiStr=%trim(%subst(vindta:413:13))
040800031124     C                   EXSR      CHKNUM
040900031124     C                   IF        PiNum=*on
041000070330     C                   EVAL      VABCAS = PiVal/10000                         * gestisco 4 decim.
041100031124     C                   ELSE
041200031124     C                   SETON                                        32
041300031124     C                   Z-ADD     *zeros        VABCAS
041400031124     C                   EVAL      vinmsg = %trimr(vinmsg)
041500031124     C                             + ' ' + 'VABCAS'
041600031124     C                   ENDIF
041700031124     C                   ENDIF
041800030929     C*
041900030929     C* Considerazioni finali su CBO/CAS
042000030929     C                   IF        FlgCAS = '1'
042100030929     C                   EVAL      VABCBO = '4'
042200030929     C                   ELSE
042300030929     C                   EVAL      VABCBO = '1'
042400030929     C                   ENDIF
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
045500020204     C                   IF        vabVAD <> 'EUR'
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
049500030606     C                   EVAL      PiDecChr = ','
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
055400021025     C                   eval      cmzfla = 'FIVAB00F'
055500021025     C                   eval      cmzmba = 'FIVAB00F'
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
