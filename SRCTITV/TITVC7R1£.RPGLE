000100040610      /TITLE Upload via Internet: traduzione in FIVABWWR. (x HIGH SPEED Cod. 0892579)
000200130314      *
000300130314      * PARTICOLARITA':
000400140529      * Su tutte le spedizione con contrassegno occorre forzare il VABTIC = 'TO'
000500130314      *
000600130122     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000700000313     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900021014     FFIVABwwr  O    E             DISK    usropn
001000000313     D*
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800130122     D tisi95ds      e ds
001900010330     D esito           s              1
002000010330     D prmlit          s             10
002100010330     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300010330     D rrnum           s              6  0 INZ(*zeros)
002400010330     D*------------------
002500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600010330     D*------------------
002700010330     D WLBDA8          DS                  INZ
002800010330     D  G08DAT                 1      8  0
002900010330     D  G08INV                 9     16  0
003000010330     D  G08ERR                17     17
003100010330     D  G08TGI                18     22  0
003200000313     D*
003300010330
003400010330
003500010330
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010601     C*
004000010601     C                   exsr      opeini
004100010605     C                   exsr      rwvab
004200130122     C*
004300130122     C* Effettuo lancio TISI95 solo x chiusura
004400130122     C                   CLEAR                   TISI95DS
004500130122     C                   EVAL      I95TLA = 'C'
004600130122     C                   CALL      'TISI95R'
004700130122     C                   PARM                    TISI95DS
004800010601     C*
004900010601     C                   seton                                        lr
005000010601
005100010601
005200010601
005300010601
005400010601     C*--------------------------------------------------------
005500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010601     C*--------------------------------------------------------
005700010601     C     PREELA        BEGSR
005800010601     C*
005900010601     C* SE OCCORRE SPEDIRE IN FILIALE
006000010601     C                   if        invfil <> *zeros and
006100010601     C                             flgGiro = '0'
006200010601     C*
006300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010601     C                   eval      flgGiro = '1'
006500010601     C*
006600010601     C                   endif
006700010601     C*
006800010601     C                   ENDSR
006900010601     C***
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010601     C*--------------------------------------------------------
007600010601     C     ENDELA        BEGSR
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000000613
008100010601
008200010601
008300010330     C*--------------------------------------------------------
008400040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008500010330     C*--------------------------------------------------------
008600010605     C     RWVAB         BEGSR
008700010330     C*
008800010330     C                   if        not %open(tivin00r)
008900010330     C                   open      tivin00r
009000010330     C                   endif
009100021014     C                   if        not %open(fivabwwr)
009200021014     C                   open      fivabwwr
009300010330     C                   endif
009400010330     C*
009500010604     C                   clear                   §CTROK
009600010604     C                   clear                   §CTRMO
009700010604     C                   clear                   §CTRNO
009800010330     C*
009900010330     C                   DO        *HIVAL
010000010330     C*
010100010330     C                   READ      tivin00r                               70
010200010618     C*
010300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010400010618     C                   if        vinflg = '1'
010500010618     C                   eval      flgOk = '1'
010600010618     C                   endif
010700010618     C*
010800010330     C                   if        vindta > *blanks
010900010330     C                   add       1             rrnum
011000010330     C*
011100010601     C                   if        *in70 = *off and
011200010330     C                             (vinflg = *blanks
011300010330     C                              or vinflg = '0'
011400010330     C                              or vinflg = '2')
011500010330     C*
011600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011700010711     C                   if        vinflg = *blanks or vinflg = '0'
011800010711     C                   clear                   vinmsg
011900010711     C                   endif
012000010601     C*
012100010330     C                   exsr      inzvar
012200010330     C                   exsr      defcam
012300010605     C                   exsr      impvab
012400010601     C*
012500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012600010605     C                   eval      depfil = VABLNP
012700010601     C                   exsr      repfil
012800010601     C                   if        depfil = invfil
012900021025     C                   if        vlrpoi = 999
013000021025     C                   MOVE(P)   invfil        VABFGS
013100021025     C                   else
013200021025     C                   MOVE(P)   vlrpoi        VABFGS
013300021025     C                   endif
013400010601     C*
013500010601     C                   exsr      PREELA
013600010601     C*
013700010604     C* Ebbene...
013800010604     C*
013900010604     C  N31              ADD       1             §CTROK            7 0
014000010604     C   32              ADD       1             §CTRMO            7 0
014100010604     C   31              ADD       1             §CTRNO            7 0
014200020205     C*
014300040301     C* Scarico il buffer del FIVAB
014400021014     C  N31              WRITE     FIVAB000
014500010604     C*
014600020722     C                   endif
014700020722     C*
014800010604     C                   if        *in31 = *off and
014900010604     C                             *in32 = *off
015000010604     C                   eval      vinflg = '1'
015100010604     C                   else
015200010604     C                   eval      vinflg = '2'
015300010604     C                   endif
015400010604     C                   endif
015500010604     C*
015600010330     C                   else
015700010330     C                   eval      vinflg = '1'
015800010330     C                   endif
015900010601     C*
016000010601     C  N70              update    tivin000
016100010330     C*
016200010330     C  N70              ENDdo
016300010601     C
016400010601     C*
016500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016600010601     C                   if        cntNonEl = *zeros or
016700010601     C                             flgMulti = '0'
016800010330     C* Se non ci sono record con errori ...
016900010601     C                   if        §ctrno = 0 and
017000010604     C                             §ctrmo = 0 and
017100010601     C                             flgStato <> '2'
017200010330     C* ... restituisco esito OK.
017300010330     C                   eval      wrkesito = '0'
017400010330     C                   else
017500010330     C                   if        §ctrok > 0
017600010330     C                   eval      wrkesito = '1'
017700010330     C                   else
017800010615     C                   if        flgOk = '0'
017900010615     C                   eval      wrkesito = '2'
018000010615     C                   else
018100010615     C                   eval      wrkesito = '6'
018200010615     C                   endif
018300010330     C                   endif
018400010330     C                   endif
018500010601     C                   else
018600010601     C                   eval      wrkesito = '9'
018700010601     C                   endif
018800010330     C*
018900010330     C                   if        %open(tivin00r)
019000010330     C                   close     tivin00r
019100010330     C                   endif
019200021014     C                   if        %open(fivabwwr)
019300021014     C                   close     fivabwwr
019400010330     C                   endif
019500010601     C*
019600010601     C                   if        vlrpoi <> 999
019700010601     C                   eval      invfil = vlrpoi
019800010601     C                   endif
019900010330     C*
020000010330     C                   if        §ctrok > 0
020100010601     C                             and invfil > *zeros
020200010330     C                   exsr      invio
020300010330     C                   endif
020400010601     C*
020500010618     C                   if        flgGiro = '1'
020600010601     C                   exsr      endela
020700010618     C                   endif
020800010330     C*
020900010330     C                   ENDSR
021000010330     C***
021100010330
021200010601
021300010601
021400010601
021500010601
021600010330     C*----------------------------------------------------*
021700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021800010330     C*----------------------------------------------------*
021900010330     C     INZVAR        BEGSR
022000010330     C*
022100020204     C                   Z-ADD     *zeros        Num5_0            5 0
022200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022300020322     C                   MOVEL     '0'           FlgCAS            1
022400010330     C*
022500010330     C                   ENDSR
022600010330     C*----------------------------------------------------*
022700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022800010330     C*----------------------------------------------------*
022900010330     C     DEFCAM        BEGSR
023000010330     C*
023100020204     C* Inizializzo il buffer del record da scrivere
023200021014     C                   CLEAR                   FIVAB000
023300020204     C* Imposto i valori di default...
023400040610     C                   EVAL      VABCCM = 0892579
023500040604     C                   EVAL      VABLNP = 089
023600040420     C                   EVAL      VABCTR = 000
023700040610     C                   EVAL      VABTSP = 'D'
023800040526     C                   EVAL      VABCBO = '1'
023900020204     C* ... e poi verifico se sono stati passati come parametri
024000020204     C                   IF        vlrppt > *blanks
024100040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500020204     C                   Z-ADD     PiVal         VABCCM
024600020204     C                   ENDIF
024700040301     C                   ENDIF
024800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025000020204     C                   EXSR      CHKNUM
025100020204     C                   IF        PiInt=*on
025200020204     C                   Z-ADD     PiVal         VABLNP
025300020204     C                   ENDIF
025400040301     C                   ENDIF
025500040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025700020204     C                   EXSR      CHKNUM
025800020204     C                   IF        PiInt=*on
025900020204     C                   Z-ADD     PiVal         VABCTR
026000040301     C                   ENDIF
026100020204     C                   ENDIF
026200020204     C                   ENDIF
026300020204     C*
026400010330     C                   ENDSR
026500010607     C*----------------------------------------------------*
026600021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026700010607     C*----------------------------------------------------*
026800010607     C     IMPVAB        BEGSR
026900010607     C*
027000010607     C                   SETOFF                                       3132
027100010607     C*
027200010607     C* Reperimento campi ALFA
027300010607     C*
027400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027500130122     C                   EVAL      VABRSD=%trim(%subst(vindta:243:30))
027600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027800020117     C* ==
027900130122     C                   EVAL      VABIND=%trim(%subst(vindta:273:30))
028000130122     C                   EVAL      VABLOD=%trim(%subst(vindta:303:30))
028100130122     C                   EVAL      VABPRD=%trim(%subst(vindta:333:2))
028200130122     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
028300150924     C                   EVAL      VABTC1=%trim(%subst(vindta:800:1))
028400130704     C                   MOVEL     *blanks       wNOTE            70
028500130704     C                   EVAL      wNOTE =%trim(%subst(vindta:550:200))
028600130704     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
028700130704     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
028800151028     C                   IF        %subst(vindta:801:1)<>*blank
028900151028     C                   EVAL      VABTSP=%trim(%subst(vindta:801:1))
029000151028     C                   ENDIF
029100151214     C                   EVAL      VABRMO=%trim(%subst(vindta:802:20))
029200010607     C*
029300010607     C* Reperimento campi NUMERICI
029400020722     C                   MOVEL     DATCOR        VABAAS
029500020722     C                   MOVE      DATCOR        VABMGS
029600150312     C* prima NSP e RMN erano valorizzate allo stesso modo con vindta:193:8
029700150312     C* adesso sono state sbinate ed RMN vie preso da una posizione di partenza diversa
029800150312     C* NSP
029900130122     C                   EVAL      PiStr=%trim(%subst(vindta:193:8))
030000020304     C                   EXSR      CHKNUM
030100020304     C                   IF        PiInt=*on
030200040301     C                   Z-ADD     PiVal         VABNSP
030300150312     C***                Z-ADD     PiVal         VABRMN
030400020304     C                   ELSE
030500040610     C                   SETON                                        32
030600020304     C                   Z-ADD     *zeros        VABNSP
030700150312     C***                Z-ADD     1             VABRMN
030800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030900150312     C                             + ' ' + 'VABNSP'
031000150312     C***                          + ' ' + 'VABNSP VABRMN'
031100020304     C                   ENDIF
031200150312     C* RMN
031300150312     C                   EVAL      PiStr=%trim(%subst(vindta:187:14))
031400150312     C                   EXSR      CHKNUM
031500150312     C                   IF        PiInt=*on
031600150312     C                   Z-ADD     PiVal         VABRMN
031700150312     C                   ELSE
031800150312     C                   SETON                                        32
031900150312     C                   Z-ADD     1             VABRMN
032000150312     C                   EVAL      vinmsg = %trimr(vinmsg)
032100150312     C                             + ' ' + 'VABRMN'
032200150312     C                   ENDIF
032300040420     C* CAD
032400130122     C                   EVAL      PiStr=%trim(%subst(vindta:343:10))
032500010607     C                   EXSR      CHKNUM
032600010607     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         Num5_0
032800020204     C                   MOVEL(p)  Num5_0        VABCAD
032900010607     C                   ELSE
033000010607     C                   SETON                                        32
033100020204     C                   EVAL      VABCAD = *zeros
033200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033300020204     C                             + ' ' + 'VABCAD'
033400010607     C                   ENDIF
033500040420     C* NCL
033600130122     C                   EVAL      PiStr=%trim(%subst(vindta:373:5))
033700041203     C                   EXSR      CHKNUM
033800041203     C                   IF        PiInt=*on
033900041203     C                   Z-ADD     PiVal         VABNCL
034000041203     C                   ELSE
034100041203     C                   SETON                                        32
034200041203     C                   Z-ADD     *zeros        VABNCL
034300041203     C                   EVAL      vinmsg = %trimr(vinmsg)
034400041203     C                             + ' ' + 'VABNCL'
034500041203     C                   ENDIF
034600040420     C* PKB
034700130122     C                   EVAL      PiStr=%trim(%subst(vindta:378:8))
034800041203     C                   EXSR      CHKNUM
034900041203     C                   IF        PiNum=*on
035000130122     C                   EVAL      VABPKB = PiVal/1000                          * gestisco 3 dec.
035100041203     C                   ELSE
035200041203     C                   SETON                                        32
035300041203     C                   Z-ADD     *zeros        VABPKB
035400041203     C                   EVAL      vinmsg = %trimr(vinmsg)
035500041203     C                             + ' ' + 'VABPKB'
035600041203     C                   ENDIF
035700040604     C* CAS
035800140529     C                   IF        %subst(vindta:413:13) <> *all'0'
035900130122     C                   EVAL      PiStr=%trim(%subst(vindta:413:13))
036000040604     C                   EXSR      CHKNUM
036100040604     C                   IF        PiNum=*on
036200130122     C                   EVAL      VABCAS = PiVal/10000                         * gestisco 4 dec.
036300040604     C                   EVAL      VABVCA = 'EUR'
036400130314     C                   EVAL      VABTIC = 'TO'
036500040604     C                   ENDIF
036600140529     C                   ENDIF
036700130122     C*
036800130122     C* Se provincia nn valorizzata la reperisco
036900130122     C* tramite TISI95R a seconda dei dati d instradamento presenti
037000130122     C                   IF        VABPRD = *blanks
037100130122     C                   CLEAR                   TISI95DS
037200130122     C                   EVAL      I95TCN = '3'
037300130122     C                   Z-ADD     datcor        I95DAT
037400130122     C                   EVAL      I95NAR = VABNZD
037500130122     C                   EVAL      I95CAP = VABCAD
037600130122     C                   EVAL      I95LOC = VABLOD
037700130122     C                   CALL      'TISI95R'
037800130122     C                   PARM                    TISI95DS
037900130122     C                   EVAL      VABPRD = O95PRV
038000130122     C                   ENDIF
038100020322     C*
038200020322     C* Considerazioni finali su CBO/CAS
038300040604     C                   IF        VABCAS > *zeros
038400040420     C                   IF        VABCBO = '1'
038500020322     C                   EVAL      VABCBO = '4'
038600040420     C                   ELSE
038700040420     C                   EVAL      VABCBO = '6'
038800040420     C                   ENDIF
038900020322     C                   ENDIF
039000020204     C*
039100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039200020204     C                   EXSR      CHKIMPDIV
039300020204     C*
039400010607     C                   ENDSR
039500010607     C*----------------------------------------------------*
039600010601
039700020204
039800020204     C*----------------------------------------------------*
039900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040000020204     C*----------------------------------------------------*
040100020204     C     CHKIMPDIV     BEGSR
040200020204     C*
040300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040400020204     C                   Z-ADD     *zeros        wrkDec            9 9
040500020204     C*
040600020204     C* Come prima cosa effettuo considerazioni sulla divisa
040700020204     C                   IF        vabIAS > *zeros
040800020204     C                   IF        vabVAS <> 'EUR'
040900020204     C                   EVAL      vabVAS =  'ITL'
041000020204     C                   ENDIF
041100020204     C                   ENDIF
041200020204     C*
041300020204     C                   IF        vabCAS > *zeros
041400020204     C                   IF        vabVCA <> 'EUR'
041500020204     C                   EVAL      vabVCA =  'ITL'
041600020204     C                   ENDIF
041700020204     C                   ENDIF
041800020204     C*
041900020204     C                   IF        vabVMD > *zeros
042000020321     C                   IF        vabVAD <> 'EUR'
042100020204     C                   EVAL      vabVAD =  'ITL'
042200020204     C                   ENDIF
042300020204     C                   ENDIF
042400020204     C*
042500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042600020204     C                   Z-ADD     vabIAS        wrkDec
042700020204     C                   IF        wrkDec > *zeros
042800020204     C                   IF        vabVAS = 'ITL'
042900020204     C                   EVAL      vabIAS = *zeros
043000020204     C                   ENDIF
043100020204     C                   ENDIF
043200020204     C*
043300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043400020204     C                   Z-ADD     vabCAS        wrkDec
043500020204     C                   IF        wrkDec > *zeros
043600020204     C                   IF        vabVCA = 'ITL'
043700020204     C                   EVAL      vabCAS = *zeros
043800020204     C                   ENDIF
043900020204     C                   ENDIF
044000020204     C*
044100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044200020204     C                   Z-ADD     vabVMD        wrkDec
044300020204     C                   IF        wrkDec > *zeros
044400020204     C                   IF        vabVAD = 'ITL'
044500020204     C                   EVAL      vabVMD = *zeros
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C                   ENDSR
045000020204     C***
045100020204
045200010330
045300010330
045400010330     C*----------------------------------------------------*
045500010330     C*  CONTROLLO NUMERICITA' CAMPI
045600010330     C*----------------------------------------------------*
045700010330     C     CHKNUM        BEGSR
045800010330     C*
045900010606     C                   IF        PiDecChr = *blanks
046000010606     C                   EVAL      PiDecChr = ','
046100010606     C                   ENDIF
046200010606     C*
046300010606     C                   CALL(e)   'ISNUMERIC'
046400010330     C                   PARM                    PiStr            30
046500010606     C                   PARM                    PiDecChr          1
046600010330     C                   PARM      *ZEROS        PiVal            30 9
046700010330     C                   PARM      '0'           PiInt             1
046800010330     C                   PARM      '0'           PiNum             1
046900010330     C                   IF        %error
047000010606     C                   EVAL      PiNum=*off
047100010330     C                   ENDIF
047200010330     C*
047300010330     C                   ENDSR
047400010330     C***
047500010330
047600010601
047700010601
047800010601
047900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048000010601     C     repfil        BEGSR
048100010601     C*
048200010601     C                   if        invfil = *zeros and
048300010601     C                             depfil > *zeros and
048400010629     C                             (vinflg = *blanks or
048500010629     C                              vinflg = *zeros)
048600010601     C
048700010601     C                   eval      invfil = depfil
048800010601     C                   endif
048900010601     C*
049000010601     C                   if        depfil <> invfil and
049100010601     C                             invfil > *zeros
049200010601     C                   eval      flgMulti = '1'
049300010601     C                   if        vinflg = *blanks
049400010601     C                   add       1             cntNonEl
049500010601     C                   endif
049600010601     C                   endif
049700010601     C*
049800010601     C                   if        vinflg = '2'
049900010601     C                   eval      flgStato = '2'
050000010601     C                   endif
050100010601     C*
050200010601     C                   ENDSR
050300010601     C***
050400010601
050500010601
050600010601
050700010330
050800010330
050900010330
051000990920      /TITLE Invio dei dati al punto operativo.
051100000613     C     invio         BEGSR
051200990920     C*
051300990920     C                   reset                   dscmz
051400010601     C                   move      invfil        cmzdst
051500990920     C                   eval      cmzfld = vlrfou
051600990920     C                   eval      cmzmbd = vlrhdl
051700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051800000710     C                   if        prmfir = *blanks
051900021014     C                   eval      cmzfla = 'FIVAB00F'
052000021014     C                   eval      cmzmba = 'FIVAB00F'
052100000710     C                   else
052200000710     C                   eval      cmzfla = prmfir
052300000710     C                   eval      cmzmba = prmfir
052400000710     C                   endif
052500990920     C                   eval      cmznrr = *zeros
052600990920     C                   move      §ctrok        cmznrr
052700021018     C                   eval      cmzlba = vlrfl1
052800990920     C                   call(e)   'TIS711C'
052900990920     C                   parm                    dscmz
053000990921     C                   parm      *blanks       esito
053100990920     C                   if        %error
053200990920     C                             or cmzerr = '1'
053300990921     C                             or esito  = '1'
053400000710     C                   eval      wrkesito = '3'
053500990920     C                   endif
053600990920     C*
053700000613     C                   ENDSR
053800990910
053900010601
054000010601
054100010601
054200010601
054300010601      /TITLE Invio dei dati al punto operativo.
054400010601     C     opeini        BEGSR
054500010601     C*
054600010601     C* Inizializzo flag e contatori operativi
054700010601     C                   movel     '0'           flgGiro           1
054800010601     C                   movel     '0'           flgMulti          1
054900010601     C                   movel     '1'           flgStato          1
055000010615     C                   movel     '0'           flgOk             1
055100010601     C                   z-add     *zeros        cntNonEl         10 0
055200010601     C                   z-add     *zeros        depfil            3 0
055300010601     C                   z-add     *zeros        invfil            3 0
055400010601     C*
055500010601     C                   ENDSR
055600010601     C***
055700010601
055800010601
055900010601
056000010330
056100010330
056200000613     C     *inzsr        BEGSR
056300990910     C*
056400990910     C     *entry        plist
056500990920     C                   parm                    tivlrds
056600990921     C                   parm      wrkesito      esito
056700000724     C                   parm                    prmlit
056800000710     C                   parm                    prmfir
056900010330     C*
057000010330     C* CALCOLA LA DATA CORRENTE
057100010330     C                   time                    wn14             14 0
057200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057300010330     C                   z-add     wn8           g08dat
057400010330     C                   z-add     *zeros        g08inv
057500010330     C                   movel     '0'           g08err
057600010330     C                   call      'XSRDA8'
057700010330     C                   parm                    wlbda8
057800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057900000613     C*
058000000613     C                   ENDSR
058100000613     C***
