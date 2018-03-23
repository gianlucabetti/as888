000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400100525     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500060316     D tisi95ds      e ds
001600100525     D ds15          e ds
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200100525
002300100525     D*------------
002400100525     D* SCHIERE
002500100525     D*------------
002600100525     D skNAZISO        S              3    DIM(1000)
002700100525     D skNAZBAR        S              3    DIM(1000)
002800100525     D jNAZ            s              5  0 INZ(*zeros)
002900100525
003000010330     D*------------------
003100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200010330     D*------------------
003300010330     D WLBDA8          DS                  INZ
003400010330     D  G08DAT                 1      8  0
003500010330     D  G08INV                 9     16  0
003600010330     D  G08ERR                17     17
003700010330     D  G08TGI                18     22  0
003800010330
003900010330
004000100525     C*
004100100525     C* Carico le tabelle
004200100525     C                   exsr      cartab
004300100525     C*
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700010601     C*
004800010601     C                   exsr      opeini
004900010605     C                   exsr      rwvab
005000060316     C*
005100060316     C* Effettuo lancio TISI95 solo x chiusura
005200060316     C                   CLEAR                   TISI95DS
005300060316     C                   EVAL      I95TLA = 'C'
005400060316     C                   CALL      'TISI95R'
005500060316     C                   PARM                    TISI95DS
005600010601     C*
005700010601     C                   seton                                        lr
005800010601
005900010601
006000010601
006100010601
006200010601     C*--------------------------------------------------------
006300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006400010601     C*--------------------------------------------------------
006500010601     C     PREELA        BEGSR
006600010601     C*
006700010601     C* SE OCCORRE SPEDIRE IN FILIALE
006800010601     C                   if        invfil <> *zeros and
006900010601     C                             flgGiro = '0'
007000010601     C*
007100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007200010601     C                   eval      flgGiro = '1'
007300010601     C*
007400010601     C                   endif
007500010601     C*
007600010601     C                   ENDSR
007700010601     C***
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008300010601     C*--------------------------------------------------------
008400010601     C     ENDELA        BEGSR
008500000616     C*
008600010601     C                   ENDSR
008700010601     C***
008800000613
008900010601
009000010601
009100010330     C*--------------------------------------------------------
009200050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
009300010330     C*--------------------------------------------------------
009400010605     C     RWVAB         BEGSR
009500050408     C*
009600010330     C                   if        not %open(tivin00r)
009700010330     C                   open      tivin00r
009800010330     C                   endif
009900050408     C                   if        not %open(fivabwwr)
010000050408     C                   open      fivabwwr
010100010330     C                   endif
010200010330     C*
010300010604     C                   clear                   §CTROK
010400010604     C                   clear                   §CTRMO
010500010604     C                   clear                   §CTRNO
010600010330     C*
010700010330     C                   DO        *HIVAL
010800010330     C*
010900010330     C                   READ      tivin00r                               70
011000010618     C*
011100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011200010618     C                   if        vinflg = '1'
011300010618     C                   eval      flgOk = '1'
011400010618     C                   endif
011500010618     C*
011600010330     C                   if        vindta > *blanks
011700010330     C                   add       1             rrnum
011800010330     C*
011900010601     C                   if        *in70 = *off and
012000010330     C                             (vinflg = *blanks
012100010330     C                              or vinflg = '0'
012200010330     C                              or vinflg = '2')
012300010330     C*
012400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012500010711     C                   if        vinflg = *blanks or vinflg = '0'
012600010711     C                   clear                   vinmsg
012700010711     C                   endif
012800010601     C*
012900010330     C                   exsr      inzvar
013000010330     C                   exsr      defcam
013100010605     C                   exsr      impvab
013200010601     C*
013300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013400010605     C                   eval      depfil = VABLNP
013500010601     C                   exsr      repfil
013600010601     C                   if        depfil = invfil
013700021025     C                   if        vlrpoi = 999
013800021025     C                   MOVE(P)   invfil        VABFGS
013900021025     C                   else
014000021025     C                   MOVE(P)   vlrpoi        VABFGS
014100021025     C                   endif
014200010601     C*
014300010601     C                   exsr      PREELA
014400010601     C*
014500010604     C* Ebbene...
014600010604     C*
014700010604     C  N31              ADD       1             §CTROK            7 0
014800010604     C   32              ADD       1             §CTRMO            7 0
014900010604     C   31              ADD       1             §CTRNO            7 0
015000020205     C*
015100050408     C* Scarico il buffer del FIVAB
015200050408     C  N31              WRITE     FIVAB000
015300010604     C*
015400020722     C                   endif
015500020722     C*
015600010604     C                   if        *in31 = *off and
015700010604     C                             *in32 = *off
015800010604     C                   eval      vinflg = '1'
015900010604     C                   else
016000010604     C                   eval      vinflg = '2'
016100010604     C                   endif
016200010604     C                   endif
016300010604     C*
016400010330     C                   else
016500010330     C                   eval      vinflg = '1'
016600010330     C                   endif
016700010601     C*
016800010601     C  N70              update    tivin000
016900010330     C*
017000010330     C  N70              ENDdo
017100010601     C*
017200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017300010601     C                   if        cntNonEl = *zeros or
017400010601     C                             flgMulti = '0'
017500010330     C* Se non ci sono record con errori ...
017600010601     C                   if        §ctrno = 0 and
017700010604     C                             §ctrmo = 0 and
017800010601     C                             flgStato <> '2'
017900010330     C* ... restituisco esito OK.
018000010330     C                   eval      wrkesito = '0'
018100010330     C                   else
018200010330     C                   if        §ctrok > 0
018300010330     C                   eval      wrkesito = '1'
018400010330     C                   else
018500010615     C                   if        flgOk = '0'
018600010615     C                   eval      wrkesito = '2'
018700010615     C                   else
018800010615     C                   eval      wrkesito = '6'
018900010615     C                   endif
019000010330     C                   endif
019100010330     C                   endif
019200010601     C                   else
019300010601     C                   eval      wrkesito = '9'
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        %open(tivin00r)
019700010330     C                   close     tivin00r
019800010330     C                   endif
019900050408     C                   if        %open(fivabwwr)
020000050408     C                   close     fivabwwr
020100010330     C                   endif
020200010601     C*
020300010601     C                   if        vlrpoi <> 999
020400010601     C                   eval      invfil = vlrpoi
020500010601     C                   endif
020600010330     C*
020700010330     C                   if        §ctrok > 0
020800010601     C                             and invfil > *zeros
020900010330     C                   exsr      invio
021000010330     C                   endif
021100010601     C*
021200010618     C                   if        flgGiro = '1'
021300010601     C                   exsr      endela
021400010618     C                   endif
021500010330     C*
021600010330     C                   ENDSR
021700010330     C***
021800010330
021900010601
022000010601
022100010601
022200010601
022300010330     C*----------------------------------------------------*
022400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022500010330     C*----------------------------------------------------*
022600010330     C     INZVAR        BEGSR
022700010330     C*
022800020204     C                   Z-ADD     *zeros        Num5_0            5 0
022900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023000020322     C                   MOVEL     '0'           FlgCAS            1
023100010330     C*
023200010330     C                   ENDSR
023300010330     C*----------------------------------------------------*
023400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023500010330     C*----------------------------------------------------*
023600010330     C     DEFCAM        BEGSR
023700010330     C*
023800020204     C* Inizializzo il buffer del record da scrivere
023900050408     C                   CLEAR                   FIVAB000
024000020204     C* Imposto i valori di default...
024100060316     C                   EVAL      VABCCM = 0062232
024200060316     C                   EVAL      VABLNP = 006
024300141114     C                   EVAL      VABCTR = 400
024400060303     C                   EVAL      VABCBO = '1'
024500020204     C* ... e poi verifico se sono stati passati come parametri
024600020204     C                   IF        vlrppt > *blanks
024700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024900020204     C                   EXSR      CHKNUM
025000020204     C                   IF        PiInt=*on
025100020204     C                   Z-ADD     PiVal         VABCCM
025200020204     C                   ENDIF
025300040301     C                   ENDIF
025400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025600020204     C                   EXSR      CHKNUM
025700020204     C                   IF        PiInt=*on
025800020204     C                   Z-ADD     PiVal         VABLNP
025900020204     C                   ENDIF
026000040301     C                   ENDIF
026100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026300020204     C                   EXSR      CHKNUM
026400020204     C                   IF        PiInt=*on
026500020204     C                   Z-ADD     PiVal         VABCTR
026600040301     C                   ENDIF
026700020204     C                   ENDIF
026800020204     C                   ENDIF
026900020204     C*
027000010330     C                   ENDSR
027100010607     C*----------------------------------------------------*
027200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027300010607     C*----------------------------------------------------*
027400010607     C     IMPVAB        BEGSR
027500010607     C*
027600010607     C                   SETOFF                                       3132
027700010607     C*
027800010607     C* Reperimento campi ALFA
027900010607     C*
028000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
028100060316     C                   EVAL      VABRSD=%trim(%subst(vindta:112:32))
028200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028400020117     C* ==
028500060316     C                   EVAL      VABIND=%trim(%subst(vindta:144:32))
028600060316     C                   EVAL      VABLOD=%trim(%subst(vindta:181:27))
028700060316     C                   EVAL      VABNOT=%trim(%subst(vindta:242:38))
028800060316     C                   EVAL      VABCBO=%trim(%subst(vindta:225:1))
028900100525     C                   EVAL      VABNZD=%trim(%subst(vindta:405:3))
029000141114     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
029100100525     C*
029200100525     C* ...passo dal codice ISO a quello "Bartolini"
029300100525     C                   Z-ADD     1             jNAZ
029400100525     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
029500100525     C                   IF        %found
029600100525     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
029700100525     C                   ENDIF
029800100525     C                   IF        VABNZD = 'IT'  OR
029900100525     C                             VABNZD = 'ITA'
030000100525     C                   EVAL      VABNZD = *blanks
030100100525     C                   ENDIF
030200010607     C*
030300010607     C* Reperimento campi NUMERICI
030400020722     C                   MOVEL     DATCOR        VABAAS
030500020722     C                   MOVE      DATCOR        VABMGS
030600060105     C* RMN/NSP
030700060316     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
030800050408     C                   EXSR      CHKNUM
030900050408     C                   IF        PiInt=*on
031000050408     C                   Z-ADD     PiVal         VABRMN
031100050908     C                   Z-ADD     PiVal         VABNSP
031200050408     C                   ELSE
031300051219     C                   SETON                                        31
031400050408     C                   Z-ADD     *zeros        VABRMN
031500050408     C                   EVAL      vinmsg = %trimr(vinmsg)
031600050908     C                             + ' ' + 'VABRMN VABNSP'
031700050408     C                   ENDIF
031800040420     C* CAD
031900060316     C                   EVAL      PiStr=%trim(%subst(vindta:176:5))
032000010607     C                   EXSR      CHKNUM
032100010607     C                   IF        PiInt=*on
032200020204     C                   Z-ADD     PiVal         Num5_0
032300020204     C                   MOVEL(p)  Num5_0        VABCAD
032400010607     C                   ELSE
032500010607     C                   SETON                                        32
032600020204     C                   EVAL      VABCAD = *zeros
032700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032800020204     C                             + ' ' + 'VABCAD'
032900010607     C                   ENDIF
033000060316     C* Reperisco la provincia dal CAP e dalla località
033100100224     C                   IF        VABNZD  = *blanks AND
033200100224     C                             VABCAD <> *blanks AND
033300060316     C                             VABPRD  = *blanks
033400060316     C                   CLEAR                   TISI95DS
033500060316     C                   EVAL      I95TCN = '3'
033600060316     C                   Z-ADD     datcor        I95DAT
033700060316     C                   EVAL      I95CAP = VABCAD
033800060316     C                   EVAL      I95LOC = VABLOD
033900060316     C                   EVAL      I95NAR = VABNZD
034000060316     C                   CALL      'TISI95R'
034100060316     C                   PARM                    TISI95DS
034200060316     C                   EVAL      VABPRD = O95PRV
034300060316     C                   ENDIF
034400040420     C* NCL
034500060316     C                   EVAL      PiStr=%trim(%subst(vindta:208:4))
034600010607     C                   EXSR      CHKNUM
034700010607     C                   IF        PiInt=*on
034800010607     C                   Z-ADD     PiVal         VABNCL
034900010607     C                   ELSE
035000010607     C                   SETON                                        32
035100010607     C                   Z-ADD     *zeros        VABNCL
035200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035300010607     C                             + ' ' + 'VABNCL'
035400010607     C                   ENDIF
035500040420     C* PKB
035600060316     C                   EVAL      PiStr=%trim(%subst(vindta:213:5))
035700010607     C                   EXSR      CHKNUM
035800010607     C                   IF        PiNum=*on
035900060316     C                   Z-ADD     PiVal         VABPKB
036000010607     C                   ELSE
036100010607     C                   SETON                                        32
036200010607     C                   Z-ADD     *zeros        VABPKB
036300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036400010607     C                             + ' ' + 'VABPKB'
036500010607     C                   ENDIF
036600070521     C* CAS
036700070521     C                   IF        %subst(vindta:227:7) <> *all'0'
036800070521     C                   EVAL      FlgCAS='1'
036900070521     C                   EVAL      VABVCA='EUR'
037000070521     C                   EVAL      PiStr=%trim(%subst(vindta:227:7))
037100070521     C                   EXSR      CHKNUM
037200070521     C                   IF        PiNum=*on
037300070521     C                   EVAL(H)   PiVal=PiVal/100
037400070521     C                   Z-ADD     PiVal         VABCAS
037500070530     C***                EVAL      VABVCA=%trim(%subst(vindta:277:3))
037600070521     C                   ELSE
037700070521     C                   SETON                                        32
037800070521     C                   EVAL      vinmsg = %trimr(vinmsg)
037900070521     C                             + ' ' + 'VABCAS'
038000070521     C                   ENDIF
038100070521     C                   ENDIF
038200141114     C* QFT
038300141114     C                   IF        %subst(vindta:336:16) <> *all'0'
038400141114     C                   EVAL      PiStr=%trim(%subst(vindta:336:16))
038500141114     C                   EXSR      CHKNUM
038600141114     C                   IF        PiNum=*on
038700141114     C                   EVAL(H)   PiVal=PiVal
038800141114     C                   Z-ADD     PiVal         VABQFT
038900141114     C                   ELSE
039000141114     C                   SETON                                        32
039100141114     C                   EVAL      vinmsg = %trimr(vinmsg)
039200141114     C                             + ' ' + 'VABQFT'
039300141114     C                   ENDIF
039400141114     C                   ENDIF
039500020322     C*
039600020322     C* Considerazioni finali su CBO/CAS
039700050908     C                   IF        FlgCAS = '1'
039800040713     C                   IF        VABCBO = '1'
039900020322     C                   EVAL      VABCBO = '4'
040000040713     C                   ENDIF
040100040713     C                   IF        VABCBO = '2'
040200040713     C                   EVAL      VABCBO = '6'
040300040713     C                   ENDIF
040400020322     C                   ENDIF
040500020204     C*
040600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040700020204     C                   EXSR      CHKIMPDIV
040800020204     C*
040900010607     C                   ENDSR
041000010607     C*----------------------------------------------------*
041100010601
041200020204
041300020204     C*----------------------------------------------------*
041400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041500020204     C*----------------------------------------------------*
041600020204     C     CHKIMPDIV     BEGSR
041700020204     C*
041800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041900020204     C                   Z-ADD     *zeros        wrkDec            9 9
042000020204     C*
042100020204     C* Come prima cosa effettuo considerazioni sulla divisa
042200020204     C                   IF        vabIAS > *zeros
042300020204     C                   IF        vabVAS <> 'EUR'
042400020204     C                   EVAL      vabVAS =  'ITL'
042500020204     C                   ENDIF
042600020204     C                   ENDIF
042700020204     C*
042800020204     C                   IF        vabCAS > *zeros
042900020204     C                   IF        vabVCA <> 'EUR'
043000020204     C                   EVAL      vabVCA =  'ITL'
043100020204     C                   ENDIF
043200020204     C                   ENDIF
043300020204     C*
043400020204     C                   IF        vabVMD > *zeros
043500020321     C                   IF        vabVAD <> 'EUR'
043600020204     C                   EVAL      vabVAD =  'ITL'
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044100020204     C                   Z-ADD     vabIAS        wrkDec
044200020204     C                   IF        wrkDec > *zeros
044300020204     C                   IF        vabVAS = 'ITL'
044400020204     C                   EVAL      vabIAS = *zeros
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044900020204     C                   Z-ADD     vabCAS        wrkDec
045000020204     C                   IF        wrkDec > *zeros
045100020204     C                   IF        vabVCA = 'ITL'
045200020204     C                   EVAL      vabCAS = *zeros
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045700020204     C                   Z-ADD     vabVMD        wrkDec
045800020204     C                   IF        wrkDec > *zeros
045900020204     C                   IF        vabVAD = 'ITL'
046000020204     C                   EVAL      vabVMD = *zeros
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C                   ENDSR
046500020204     C***
046600020204
046700010330
046800010330
046900010330     C*----------------------------------------------------*
047000010330     C*  CONTROLLO NUMERICITA' CAMPI
047100010330     C*----------------------------------------------------*
047200010330     C     CHKNUM        BEGSR
047300010330     C*
047400010606     C                   IF        PiDecChr = *blanks
047500010606     C                   EVAL      PiDecChr = ','
047600010606     C                   ENDIF
047700010606     C*
047800010606     C                   CALL(e)   'ISNUMERIC'
047900010330     C                   PARM                    PiStr            30
048000010606     C                   PARM                    PiDecChr          1
048100010330     C                   PARM      *ZEROS        PiVal            30 9
048200010330     C                   PARM      '0'           PiInt             1
048300010330     C                   PARM      '0'           PiNum             1
048400010330     C                   IF        %error
048500010606     C                   EVAL      PiNum=*off
048600010330     C                   ENDIF
048700010330     C*
048800010330     C                   ENDSR
048900010330     C***
049000010330
049100010601
049200010601
049300010601
049400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049500010601     C     repfil        BEGSR
049600010601     C*
049700010601     C                   if        invfil = *zeros and
049800010601     C                             depfil > *zeros and
049900010629     C                             (vinflg = *blanks or
050000010629     C                              vinflg = *zeros)
050100010601     C
050200010601     C                   eval      invfil = depfil
050300010601     C                   endif
050400010601     C*
050500010601     C                   if        depfil <> invfil and
050600010601     C                             invfil > *zeros
050700010601     C                   eval      flgMulti = '1'
050800010601     C                   if        vinflg = *blanks
050900010601     C                   add       1             cntNonEl
051000010601     C                   endif
051100010601     C                   endif
051200010601     C*
051300010601     C                   if        vinflg = '2'
051400010601     C                   eval      flgStato = '2'
051500010601     C                   endif
051600010601     C*
051700010601     C                   ENDSR
051800010601     C***
051900010601
052000010601
052100010601
052200010330
052300010330
052400010330
052500990920      /TITLE Invio dei dati al punto operativo.
052600000613     C     invio         BEGSR
052700990920     C*
052800990920     C                   reset                   dscmz
052900010601     C                   move      invfil        cmzdst
053000990920     C                   eval      cmzfld = vlrfou
053100990920     C                   eval      cmzmbd = vlrhdl
053200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
053300000710     C                   if        prmfir = *blanks
053400050408     C                   eval      cmzfla = 'FIVAB00F'
053500050408     C                   eval      cmzmba = 'FIVAB00F'
053600000710     C                   else
053700000710     C                   eval      cmzfla = prmfir
053800000710     C                   eval      cmzmba = prmfir
053900000710     C                   endif
054000990920     C                   eval      cmznrr = *zeros
054100990920     C                   move      §ctrok        cmznrr
054200021018     C                   eval      cmzlba = vlrfl1
054300990920     C                   call(e)   'TIS711C'
054400990920     C                   parm                    dscmz
054500990921     C                   parm      *blanks       esito
054600990920     C                   if        %error
054700990920     C                             or cmzerr = '1'
054800990921     C                             or esito  = '1'
054900000710     C                   eval      wrkesito = '3'
055000990920     C                   endif
055100990920     C*
055200000613     C                   ENDSR
055300990910
055400010601
055500010601
055600010601
055700010601
055800010601      /TITLE Invio dei dati al punto operativo.
055900010601     C     opeini        BEGSR
056000010601     C*
056100010601     C* Inizializzo flag e contatori operativi
056200010601     C                   movel     '0'           flgGiro           1
056300010601     C                   movel     '0'           flgMulti          1
056400010601     C                   movel     '1'           flgStato          1
056500010615     C                   movel     '0'           flgOk             1
056600010601     C                   z-add     *zeros        cntNonEl         10 0
056700010601     C                   z-add     *zeros        depfil            3 0
056800010601     C                   z-add     *zeros        invfil            3 0
056900010601     C*
057000010601     C                   ENDSR
057100010601     C***
057200010601
057300100525
057400100525     C*--------------------------------------------------------
057500100525     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
057600100525     C*--------------------------------------------------------
057700100525     C     CARTAB        BEGSR
057800100525     C*
057900100525     C* TABELLA '15' - NAZIONI
058000100525     C                   clear                   skNAZISO
058100100525     C                   clear                   skNAZBAR
058200100525     C                   eval      tblKUT = 1
058300100525     C                   eval      tblCOD = '15'
058400100525     C     KEYtabP       setll     tabel00f
058500100525     C     KEYtabP       reade     tabel00f
058600100525     C                   dow       not %eof(tabel00f)
058700100525     C                   if        tblFLG = *blanks
058800100525     C                   movel(p)  tblUNI        ds15
058900100525     C                   add       1             jNAZ
059000100525     C                   eval      skNAZBAR(jNAZ) = tblKEY
059100100525     C                   eval      skNAZISO(jNAZ) = §15COD
059200100525     C                   endif
059300100525     C     KEYtabP       reade     tabel00f
059400100525     C                   enddo
059500100525     C*
059600100525     C                   ENDSR
059700100525     C***
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
060800100525     C*
060900100525     C* Chiave su TABEL00F - parziale
061000100525     C     KEYtabP       KLIST
061100100525     C                   KFLD                    tblKUT
061200100525     C                   KFLD                    tblCOD
061300010330     C*
061400010330     C* CALCOLA LA DATA CORRENTE
061500010330     C                   time                    wn14             14 0
061600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061700010330     C                   z-add     wn8           g08dat
061800010330     C                   z-add     *zeros        g08inv
061900010330     C                   movel     '0'           g08err
062000010330     C                   call      'XSRDA8'
062100010330     C                   parm                    wlbda8
062200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062300000613     C*
062400000613     C                   ENDSR
062500000613     C***
