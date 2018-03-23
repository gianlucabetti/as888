000100041103      /TITLE Upload via Internet: traduzione in FIVABWWR. (x EMMEGI Cod. 0010051)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021014     FFIVABwwr  O    E             DISK    usropn
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
001900041103     d wdcr            s              8  0
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800000313     D*
002900010330
003000041103     D trul13ds      e ds                  inz
003100010330
003200010330
003300000913     C                   reset                   rrnum
003400990921     C                   reset                   esito
003500990921     C                   reset                   wrkesito
003600010601     C*
003700010601     C                   exsr      opeini
003800010605     C                   exsr      rwvab
003900010601     C*
004000010601     C                   seton                                        lr
004100010601
004200010601
004300010601
004400010601
004500010601     C*--------------------------------------------------------
004600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004700010601     C*--------------------------------------------------------
004800010601     C     PREELA        BEGSR
004900010601     C*
005000010601     C* SE OCCORRE SPEDIRE IN FILIALE
005100010601     C                   if        invfil <> *zeros and
005200010601     C                             flgGiro = '0'
005300010601     C*
005400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005500010601     C                   eval      flgGiro = '1'
005600010601     C*
005700010601     C                   endif
005800010601     C*
005900010601     C                   ENDSR
006000010601     C***
006100010601
006200010601
006300010601
006400010601     C*--------------------------------------------------------
006500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006600010601     C*--------------------------------------------------------
006700010601     C     ENDELA        BEGSR
006800000616     C*
006900010601     C                   ENDSR
007000010601     C***
007100000613
007200010601
007300010601
007400010330     C*--------------------------------------------------------
007500040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
007600010330     C*--------------------------------------------------------
007700010605     C     RWVAB         BEGSR
007800010330     C*
007900010330     C                   if        not %open(tivin00r)
008000010330     C                   open      tivin00r
008100010330     C                   endif
008200021014     C                   if        not %open(fivabwwr)
008300021014     C                   open      fivabwwr
008400010330     C                   endif
008500010330     C*
008600010604     C                   clear                   §CTROK
008700010604     C                   clear                   §CTRMO
008800010604     C                   clear                   §CTRNO
008900010330     C*
009000010330     C                   DO        *HIVAL
009100010330     C*
009200010330     C                   READ      tivin00r                               70
009300010618     C*
009400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009500010618     C                   if        vinflg = '1'
009600010618     C                   eval      flgOk = '1'
009700010618     C                   endif
009800010618     C*
009900010330     C                   if        vindta > *blanks
010000010330     C                   add       1             rrnum
010100010330     C*
010200010601     C                   if        *in70 = *off and
010300010330     C                             (vinflg = *blanks
010400010330     C                              or vinflg = '0'
010500010330     C                              or vinflg = '2')
010600010330     C*
010700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010800010711     C                   if        vinflg = *blanks or vinflg = '0'
010900010711     C                   clear                   vinmsg
011000010711     C                   endif
011100010601     C*
011200010330     C                   exsr      inzvar
011300010330     C                   exsr      defcam
011400010605     C                   exsr      impvab
011500010601     C*
011600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011700010605     C                   eval      depfil = VABLNP
011800010601     C                   exsr      repfil
011900010601     C                   if        depfil = invfil
012000021025     C                   if        vlrpoi = 999
012100021025     C                   MOVE(P)   invfil        VABFGS
012200021025     C                   else
012300021025     C                   MOVE(P)   vlrpoi        VABFGS
012400021025     C                   endif
012500010601     C*
012600010601     C                   exsr      PREELA
012700010601     C*
012800010604     C* Ebbene...
012900010604     C*
013000010604     C  N31              ADD       1             §CTROK            7 0
013100010604     C   32              ADD       1             §CTRMO            7 0
013200010604     C   31              ADD       1             §CTRNO            7 0
013300020205     C*
013400040301     C* Scarico il buffer del FIVAB
013500021014     C  N31              WRITE     FIVAB000
013600010604     C*
013700020722     C                   endif
013800020722     C*
013900010604     C                   if        *in31 = *off and
014000010604     C                             *in32 = *off
014100010604     C                   eval      vinflg = '1'
014200010604     C                   else
014300010604     C                   eval      vinflg = '2'
014400010604     C                   endif
014500010604     C                   endif
014600010604     C*
014700010330     C                   else
014800010330     C                   eval      vinflg = '1'
014900010330     C                   endif
015000010601     C*
015100010601     C  N70              update    tivin000
015200010330     C*
015300010330     C  N70              ENDdo
015400010601     C
015500010601     C*
015600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015700010601     C                   if        cntNonEl = *zeros or
015800010601     C                             flgMulti = '0'
015900010330     C* Se non ci sono record con errori ...
016000010601     C                   if        §ctrno = 0 and
016100010604     C                             §ctrmo = 0 and
016200010601     C                             flgStato <> '2'
016300010330     C* ... restituisco esito OK.
016400010330     C                   eval      wrkesito = '0'
016500010330     C                   else
016600010330     C                   if        §ctrok > 0
016700010330     C                   eval      wrkesito = '1'
016800010330     C                   else
016900010615     C                   if        flgOk = '0'
017000010615     C                   eval      wrkesito = '2'
017100010615     C                   else
017200010615     C                   eval      wrkesito = '6'
017300010615     C                   endif
017400010330     C                   endif
017500010330     C                   endif
017600010601     C                   else
017700010601     C                   eval      wrkesito = '9'
017800010601     C                   endif
017900010330     C*
018000010330     C                   if        %open(tivin00r)
018100010330     C                   close     tivin00r
018200010330     C                   endif
018300021014     C                   if        %open(fivabwwr)
018400021014     C                   close     fivabwwr
018500010330     C                   endif
018600010601     C*
018700010601     C                   if        vlrpoi <> 999
018800010601     C                   eval      invfil = vlrpoi
018900010601     C                   endif
019000010330     C*
019100010330     C                   if        §ctrok > 0
019200010601     C                             and invfil > *zeros
019300010330     C                   exsr      invio
019400010330     C                   endif
019500010601     C*
019600010618     C                   if        flgGiro = '1'
019700010601     C                   exsr      endela
019800010618     C                   endif
019900010330     C*
020000010330     C                   ENDSR
020100010330     C***
020200010330
020300010601
020400010601
020500010601
020600010601
020700010330     C*----------------------------------------------------*
020800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900010330     C*----------------------------------------------------*
021000010330     C     INZVAR        BEGSR
021100010330     C*
021200020204     C                   Z-ADD     *zeros        Num5_0            5 0
021300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021400020322     C                   MOVEL     '0'           FlgCAS            1
021500010330     C*
021600010330     C                   ENDSR
021700010330     C*----------------------------------------------------*
021800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021900010330     C*----------------------------------------------------*
022000010330     C     DEFCAM        BEGSR
022100010330     C*
022200020204     C* Inizializzo il buffer del record da scrivere
022300021014     C                   CLEAR                   FIVAB000
022400020204     C* Imposto i valori di default...
022500041103     C                   EVAL      VABCCM = 0010051
022600041103     C                   EVAL      VABLNP = 001
022700040713     C                   EVAL      VABCTR = 000
022800041108     C                   EVAL      VABCBO = '1'
022900020204     C* ... e poi verifico se sono stati passati come parametri
023000020204     C                   IF        vlrppt > *blanks
023100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023300020204     C                   EXSR      CHKNUM
023400020204     C                   IF        PiInt=*on
023500020204     C                   Z-ADD     PiVal         VABCCM
023600020204     C                   ENDIF
023700040301     C                   ENDIF
023800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024000020204     C                   EXSR      CHKNUM
024100020204     C                   IF        PiInt=*on
024200020204     C                   Z-ADD     PiVal         VABLNP
024300020204     C                   ENDIF
024400040301     C                   ENDIF
024500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024700020204     C                   EXSR      CHKNUM
024800020204     C                   IF        PiInt=*on
024900020204     C                   Z-ADD     PiVal         VABCTR
025000040301     C                   ENDIF
025100020204     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C*
025400010330     C                   ENDSR
025500010607     C*----------------------------------------------------*
025600021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025700010607     C*----------------------------------------------------*
025800010607     C     IMPVAB        BEGSR
025900010607     C*
026000010607     C                   SETOFF                                       3132
026100010607     C*
026200010607     C* Reperimento campi ALFA
026300010607     C*
026400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026500041103     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
026600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026800020117     C* ==
026900041103     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
027000041103     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
027100041103     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
027200041103     C                   EVAL      VABRMA=%trim(%subst(vindta:114:4))
027300041103     C                   EVAL      VABCBO=%trim(%subst(vindta:207:1))
027400090610     C                   EVAL      VABNOT=%trim(%subst(vindta:221:35))
027500090610     C                   EVAL      VABNT2=%trim(%subst(vindta:256:35))
027600041103      * TIC
027700100305     C                   SELECT
027800100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '001' OR
027900100305     C                             %trim(%subst(vindta:208:3)) = '002'
028000100305     C                   CLEAR                   VABTIC
028100100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '010'
028200100305     C                   EVAL      VABTIC = 'BM'
028300100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '012'
028400100305     C                   EVAL      VABTIC = 'TM'
028500100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '013'
028600100305     C                   EVAL      VABTC1 = 'A'
028700100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '016'
028800100305     C                   EVAL      VABTC1 = 'P'
028900100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '018'
029000100305     C                   EVAL      VABTC1 = 'P'
029100100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '019'
029200100305     C                   EVAL      VABTIC = 'BM'
029300100305     C                   EVAL      VABDCR = %dec(%date() + %days(1) : *ISO)
029400100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '021'
029500100305     C                   EVAL      VABTIC = 'BM'
029600100305     C                   EVAL      VABTC1 = 'A'
029700100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '022'
029800100305     C                   EVAL      VABTIC = *blanks
029900100305     C                   EVAL      VABTC1 = 'A'
030000100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '023'
030100100317     C                   EVAL      VABTIC = 'TM'
030200100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '024'
030300100305     C                   EVAL      VABTSP = 'E'
030400100305     C                   WHEN      %trim(%subst(vindta:208:3)) = '025'
030500100305     C                   EVAL      VABTSP = 'H'
030600100408     C                   WHEN      %trim(%subst(vindta:208:3)) = '014'
030700100408     C                   EVAL      VABTIC = 'BM'
030800100408     C                   EVAL      VABTC1 = 'A'
030900100305     C                   ENDSL
031000010607     C*
031100010607     C* Reperimento campi NUMERICI
031200020722     C                   MOVEL     DATCOR        VABAAS
031300020722     C                   MOVE      DATCOR        VABMGS
031400040420     C* NSP/RMN
031500041122     C                   EVAL      PiStr=%trim(%subst(vindta:113:5))
031600020304     C                   EXSR      CHKNUM
031700020304     C                   IF        PiInt=*on
031800040301     C                   Z-ADD     PiVal         VABNSP
031900040301     C                   Z-ADD     PiVal         VABRMN
032000020304     C                   ELSE
032100020722     C                   SETON                                        31
032200020304     C                   Z-ADD     *zeros        VABNSP
032300040301     C                   Z-ADD     *zeros        VABRMN
032400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
032500040301     C                             + ' ' + 'VABNSP VABRMN'
032600020304     C                   ENDIF
032700040420     C* CAD
032800041103     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
032900010607     C                   EXSR      CHKNUM
033000010607     C                   IF        PiInt=*on
033100020204     C                   Z-ADD     PiVal         Num5_0
033200020204     C                   MOVEL(p)  Num5_0        VABCAD
033300010607     C                   ELSE
033400010607     C                   SETON                                        32
033500020204     C                   EVAL      VABCAD = *zeros
033600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033700020204     C                             + ' ' + 'VABCAD'
033800010607     C                   ENDIF
033900040420     C* NCL
034000041103     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
034100010607     C                   EXSR      CHKNUM
034200010607     C                   IF        PiInt=*on
034300010607     C                   Z-ADD     PiVal         VABNCL
034400010607     C                   ELSE
034500010607     C                   SETON                                        32
034600010607     C                   Z-ADD     *zeros        VABNCL
034700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034800010607     C                             + ' ' + 'VABNCL'
034900010607     C                   ENDIF
035000040420     C* PKB
035100041103     C                   EVAL      PiStr=%trim(%subst(vindta:129:8))
035200010607     C                   EXSR      CHKNUM
035300010607     C                   IF        PiNum=*on
035400040420     C                   Z-ADD     PiVal         VABPKB
035500010607     C                   ELSE
035600010607     C                   SETON                                        32
035700010607     C                   Z-ADD     *zeros        VABPKB
035800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035900010607     C                             + ' ' + 'VABPKB'
036000010607     C                   ENDIF
036100041103     C* CAS
036200041103     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
036300041103     C                   EXSR      CHKNUM
036400041103     C                   IF        PiNum=*on
036500041103     C                   Z-ADD     PiVal         VABCAS
036600041103     C                   IF        vabcas > *Zeros
036700041103     C                   MOVEL     '1'           FlgCAS
036800041108     C                   EVAL      VABVCA = 'EUR'
036900041103     C                   ENDIF
037000041103     C                   ELSE
037100041103     C                   SETON                                        32
037200041103     C                   Z-ADD     *zeros        VABCAS
037300041103     C                   EVAL      vinmsg = %trimr(vinmsg)
037400041103     C                             + ' ' + 'VABCAS'
037500041103     C                   ENDIF
037600090402     C* DCR
037700041108     C                   IF        %trim(%subst(vindta:208:3)) = '003'
037800041103     C                   EVAL      VABDCR = wdcr
037900041103     C                   ENDIF
038000090402     C* IAS
038100090402     C                   IF        %trim(%subst(vindta:208:3)) = '008'
038200090402     C                   EVAL      VABVAS = 'EUR'
038300090610     C                   EVAL      PiStr=%trim(%subst(vindta:211:10))
038400090402     C                   EXSR      CHKNUM
038500090402     C                   IF        PiNum=*on
038600090402     C                   Z-ADD     PiVal         VABIAS
038700090402     C                   ELSE
038800090402     C                   SETON                                        32
038900090402     C                   Z-ADD     *zeros        VABIAS
039000090402     C                   EVAL      vinmsg = %trimr(vinmsg)
039100090402     C                             + ' ' + 'VABIAS'
039200090402     C                   ENDIF
039300090402     C                   ENDIF
039400020322     C*
039500020322     C* Considerazioni finali su CBO/CAS
039600020322     C                   IF        FlgCAS = '1'
039700040713     C                   IF        VABCBO = '1'
039800020322     C                   EVAL      VABCBO = '4'
039900040713     C                   ENDIF
040000040713     C                   IF        VABCBO = '2'
040100040713     C                   EVAL      VABCBO = '6'
040200040713     C                   ENDIF
040300020322     C                   ENDIF
040400020204     C*
040500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040600020204     C                   EXSR      CHKIMPDIV
040700020204     C*
040800010607     C                   ENDSR
040900010607     C*----------------------------------------------------*
041000010601
041100020204
041200020204     C*----------------------------------------------------*
041300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041400020204     C*----------------------------------------------------*
041500020204     C     CHKIMPDIV     BEGSR
041600020204     C*
041700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041800020204     C                   Z-ADD     *zeros        wrkDec            9 9
041900020204     C*
042000020204     C* Come prima cosa effettuo considerazioni sulla divisa
042100020204     C                   IF        vabIAS > *zeros
042200020204     C                   IF        vabVAS <> 'EUR'
042300020204     C                   EVAL      vabVAS =  'ITL'
042400020204     C                   ENDIF
042500020204     C                   ENDIF
042600020204     C*
042700020204     C                   IF        vabCAS > *zeros
042800020204     C                   IF        vabVCA <> 'EUR'
042900020204     C                   EVAL      vabVCA =  'ITL'
043000020204     C                   ENDIF
043100020204     C                   ENDIF
043200020204     C*
043300020204     C                   IF        vabVMD > *zeros
043400020321     C                   IF        vabVAD <> 'EUR'
043500020204     C                   EVAL      vabVAD =  'ITL'
043600020204     C                   ENDIF
043700020204     C                   ENDIF
043800020204     C*
043900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044000020204     C                   Z-ADD     vabIAS        wrkDec
044100020204     C                   IF        wrkDec > *zeros
044200020204     C                   IF        vabVAS = 'ITL'
044300020204     C                   EVAL      vabIAS = *zeros
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044800020204     C                   Z-ADD     vabCAS        wrkDec
044900020204     C                   IF        wrkDec > *zeros
045000020204     C                   IF        vabVCA = 'ITL'
045100020204     C                   EVAL      vabCAS = *zeros
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045600020204     C                   Z-ADD     vabVMD        wrkDec
045700020204     C                   IF        wrkDec > *zeros
045800020204     C                   IF        vabVAD = 'ITL'
045900020204     C                   EVAL      vabVMD = *zeros
046000020204     C                   ENDIF
046100020204     C                   ENDIF
046200020204     C*
046300020204     C                   ENDSR
046400020204     C***
046500020204
046600010330
046700010330
046800010330     C*----------------------------------------------------*
046900010330     C*  CONTROLLO NUMERICITA' CAMPI
047000010330     C*----------------------------------------------------*
047100010330     C     CHKNUM        BEGSR
047200010330     C*
047300010606     C                   IF        PiDecChr = *blanks
047400010606     C                   EVAL      PiDecChr = ','
047500010606     C                   ENDIF
047600010606     C*
047700010606     C                   CALL(e)   'ISNUMERIC'
047800010330     C                   PARM                    PiStr            30
047900010606     C                   PARM                    PiDecChr          1
048000010330     C                   PARM      *ZEROS        PiVal            30 9
048100010330     C                   PARM      '0'           PiInt             1
048200010330     C                   PARM      '0'           PiNum             1
048300010330     C                   IF        %error
048400010606     C                   EVAL      PiNum=*off
048500010330     C                   ENDIF
048600010330     C*
048700010330     C                   ENDSR
048800010330     C***
048900010330
049000010601
049100010601
049200010601
049300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049400010601     C     repfil        BEGSR
049500010601     C*
049600010601     C                   if        invfil = *zeros and
049700010601     C                             depfil > *zeros and
049800010629     C                             (vinflg = *blanks or
049900010629     C                              vinflg = *zeros)
050000010601     C
050100010601     C                   eval      invfil = depfil
050200010601     C                   endif
050300010601     C*
050400010601     C                   if        depfil <> invfil and
050500010601     C                             invfil > *zeros
050600010601     C                   eval      flgMulti = '1'
050700010601     C                   if        vinflg = *blanks
050800010601     C                   add       1             cntNonEl
050900010601     C                   endif
051000010601     C                   endif
051100010601     C*
051200010601     C                   if        vinflg = '2'
051300010601     C                   eval      flgStato = '2'
051400010601     C                   endif
051500010601     C*
051600010601     C                   ENDSR
051700010601     C***
051800010601
051900010601
052000010601
052100010330
052200010330
052300010330
052400990920      /TITLE Invio dei dati al punto operativo.
052500000613     C     invio         BEGSR
052600990920     C*
052700990920     C                   reset                   dscmz
052800010601     C                   move      invfil        cmzdst
052900990920     C                   eval      cmzfld = vlrfou
053000990920     C                   eval      cmzmbd = vlrhdl
053100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
053200000710     C                   if        prmfir = *blanks
053300021014     C                   eval      cmzfla = 'FIVAB00F'
053400021014     C                   eval      cmzmba = 'FIVAB00F'
053500000710     C                   else
053600000710     C                   eval      cmzfla = prmfir
053700000710     C                   eval      cmzmba = prmfir
053800000710     C                   endif
053900990920     C                   eval      cmznrr = *zeros
054000990920     C                   move      §ctrok        cmznrr
054100021018     C                   eval      cmzlba = vlrfl1
054200990920     C                   call(e)   'TIS711C'
054300990920     C                   parm                    dscmz
054400990921     C                   parm      *blanks       esito
054500990920     C                   if        %error
054600990920     C                             or cmzerr = '1'
054700990921     C                             or esito  = '1'
054800000710     C                   eval      wrkesito = '3'
054900990920     C                   endif
055000990920     C*
055100000613     C                   ENDSR
055200990910
055300010601
055400010601
055500010601
055600010601
055700010601      /TITLE Invio dei dati al punto operativo.
055800010601     C     opeini        BEGSR
055900010601     C*
056000010601     C* Inizializzo flag e contatori operativi
056100010601     C                   movel     '0'           flgGiro           1
056200010601     C                   movel     '0'           flgMulti          1
056300010601     C                   movel     '1'           flgStato          1
056400010615     C                   movel     '0'           flgOk             1
056500010601     C                   z-add     *zeros        cntNonEl         10 0
056600010601     C                   z-add     *zeros        depfil            3 0
056700010601     C                   z-add     *zeros        invfil            3 0
056800010601     C*
056900010601     C                   ENDSR
057000010601     C***
057100010601
057200010601
057300010601
057400010330
057500010330
057600000613     C     *inzsr        BEGSR
057700990910     C*
057800990910     C     *entry        plist
057900990920     C                   parm                    tivlrds
058000990921     C                   parm      wrkesito      esito
058100000724     C                   parm                    prmlit
058200000710     C                   parm                    prmfir
058300010330     C*
058400010330     C* CALCOLA LA DATA CORRENTE
058500010330     C                   time                    wn14             14 0
058600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058700010330     C                   z-add     wn8           g08dat
058800010330     C                   z-add     *zeros        g08inv
058900010330     C                   movel     '0'           g08err
059000010330     C                   call      'XSRDA8'
059100010330     C                   parm                    wlbda8
059200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059300041103
059400041103      * calcolo già oggi + 1 gg lavorativo per la data consegna richiesta
059500041103     C                   clear                   TRUL13DS
059600041103     C                   eval      I13MOD = 'P'
059700041103     C                   eval      I13FIL = 001
059800041103     C                   eval      I13DIN = datcor
059900041103     C                   Movel     wn14          I13HIN
060000041103     C                   eval      I13GIO = 1
060100041103     C                   eval      I13ORE = *zeros
060200041103     C                   call      'TRUL13R'
060300041103     C                   parm                    TRUL13DS
060400041103     C                   if        O13ERR = *blanks
060500041103     C                   eval      wdcr  = O13DFI
060600041103     C                   else
060700041103     C                   eval      wdcr  = datcor
060800041103     C                   endif
060900000613     C*
061000000613     C                   ENDSR
061100000613     C***
