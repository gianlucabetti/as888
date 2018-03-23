000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
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
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300010601     C                   exsr      opeini
003400010605     C                   exsr      rwvab
003500010601     C*
003600010601     C                   seton                                        lr
003700010601
003800010601
003900010601
004000010601
004100010601     C*--------------------------------------------------------
004200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004300010601     C*--------------------------------------------------------
004400010601     C     PREELA        BEGSR
004500010601     C*
004600010601     C* SE OCCORRE SPEDIRE IN FILIALE
004700010601     C                   if        invfil <> *zeros and
004800010601     C                             flgGiro = '0'
004900010601     C*
005000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005100010601     C                   eval      flgGiro = '1'
005200010601     C*
005300010601     C                   endif
005400010601     C*
005500010601     C                   ENDSR
005600010601     C***
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006200010601     C*--------------------------------------------------------
006300010601     C     ENDELA        BEGSR
006400000616     C*
006500010601     C                   ENDSR
006600010601     C***
006700000613
006800010601
006900010601
007000010330     C*--------------------------------------------------------
007100050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
007200010330     C*--------------------------------------------------------
007300010605     C     RWVAB         BEGSR
007400050408     C*
007500010330     C                   if        not %open(tivin00r)
007600010330     C                   open      tivin00r
007700010330     C                   endif
007800050408     C                   if        not %open(fivabwwr)
007900050408     C                   open      fivabwwr
008000010330     C                   endif
008100010330     C*
008200010604     C                   clear                   §CTROK
008300010604     C                   clear                   §CTRMO
008400010604     C                   clear                   §CTRNO
008500010330     C*
008600010330     C                   DO        *HIVAL
008700010330     C*
008800010330     C                   READ      tivin00r                               70
008900010618     C*
009000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009100010618     C                   if        vinflg = '1'
009200010618     C                   eval      flgOk = '1'
009300010618     C                   endif
009400010618     C*
009500010330     C                   if        vindta > *blanks
009600010330     C                   add       1             rrnum
009700010330     C*
009800010601     C                   if        *in70 = *off and
009900010330     C                             (vinflg = *blanks
010000010330     C                              or vinflg = '0'
010100010330     C                              or vinflg = '2')
010200010330     C*
010300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010400010711     C                   if        vinflg = *blanks or vinflg = '0'
010500010711     C                   clear                   vinmsg
010600010711     C                   endif
010700010601     C*
010800010330     C                   exsr      inzvar
010900010330     C                   exsr      defcam
011000010605     C                   exsr      impvab
011100010601     C*
011200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011300010605     C                   eval      depfil = VABLNP
011400010601     C                   exsr      repfil
011500010601     C                   if        depfil = invfil
011600021025     C                   if        vlrpoi = 999
011700021025     C                   MOVE(P)   invfil        VABFGS
011800021025     C                   else
011900021025     C                   MOVE(P)   vlrpoi        VABFGS
012000021025     C                   endif
012100010601     C*
012200010601     C                   exsr      PREELA
012300010601     C*
012400010604     C* Ebbene...
012500010604     C*
012600010604     C  N31              ADD       1             §CTROK            7 0
012700010604     C   32              ADD       1             §CTRMO            7 0
012800010604     C   31              ADD       1             §CTRNO            7 0
012900020205     C*
013000050408     C* Scarico il buffer del FIVAB
013100050408     C  N31              WRITE     FIVAB000
013200010604     C*
013300020722     C                   endif
013400020722     C*
013500010604     C                   if        *in31 = *off and
013600010604     C                             *in32 = *off
013700010604     C                   eval      vinflg = '1'
013800010604     C                   else
013900010604     C                   eval      vinflg = '2'
014000010604     C                   endif
014100010604     C                   endif
014200010604     C*
014300010330     C                   else
014400010330     C                   eval      vinflg = '1'
014500010330     C                   endif
014600010601     C*
014700010601     C  N70              update    tivin000
014800010330     C*
014900010330     C  N70              ENDdo
015000010601     C*
015100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015200010601     C                   if        cntNonEl = *zeros or
015300010601     C                             flgMulti = '0'
015400010330     C* Se non ci sono record con errori ...
015500010601     C                   if        §ctrno = 0 and
015600010604     C                             §ctrmo = 0 and
015700010601     C                             flgStato <> '2'
015800010330     C* ... restituisco esito OK.
015900010330     C                   eval      wrkesito = '0'
016000010330     C                   else
016100010330     C                   if        §ctrok > 0
016200010330     C                   eval      wrkesito = '1'
016300010330     C                   else
016400010615     C                   if        flgOk = '0'
016500010615     C                   eval      wrkesito = '2'
016600010615     C                   else
016700010615     C                   eval      wrkesito = '6'
016800010615     C                   endif
016900010330     C                   endif
017000010330     C                   endif
017100010601     C                   else
017200010601     C                   eval      wrkesito = '9'
017300010601     C                   endif
017400010330     C*
017500010330     C                   if        %open(tivin00r)
017600010330     C                   close     tivin00r
017700010330     C                   endif
017800050408     C                   if        %open(fivabwwr)
017900050408     C                   close     fivabwwr
018000010330     C                   endif
018100010601     C*
018200010601     C                   if        vlrpoi <> 999
018300010601     C                   eval      invfil = vlrpoi
018400010601     C                   endif
018500010330     C*
018600010330     C                   if        §ctrok > 0
018700010601     C                             and invfil > *zeros
018800010330     C                   exsr      invio
018900010330     C                   endif
019000010601     C*
019100010618     C                   if        flgGiro = '1'
019200010601     C                   exsr      endela
019300010618     C                   endif
019400010330     C*
019500010330     C                   ENDSR
019600010330     C***
019700010330
019800010601
019900010601
020000010601
020100010601
020200010330     C*----------------------------------------------------*
020300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020400010330     C*----------------------------------------------------*
020500010330     C     INZVAR        BEGSR
020600010330     C*
020700020204     C                   Z-ADD     *zeros        Num5_0            5 0
020800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
020900020322     C                   MOVEL     '0'           FlgCAS            1
021000010330     C*
021100010330     C                   ENDSR
021200010330     C*----------------------------------------------------*
021300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021400010330     C*----------------------------------------------------*
021500010330     C     DEFCAM        BEGSR
021600010330     C*
021700020204     C* Inizializzo il buffer del record da scrivere
021800050408     C                   CLEAR                   FIVAB000
021900020204     C* Imposto i valori di default...
022000060112     C                   EVAL      VABCCM = 2220282
022100060112     C                   EVAL      VABLNP = 222
022200060105     C                   EVAL      VABCTR = 000
022300070117      * imposto il vabctm perchè il cliente manda anche il packing list
022400070117     c                   eval      vabctm = '7A'
022500020204     C* ... e poi verifico se sono stati passati come parametri
022600020204     C                   IF        vlrppt > *blanks
022700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022900020204     C                   EXSR      CHKNUM
023000020204     C                   IF        PiInt=*on
023100020204     C                   Z-ADD     PiVal         VABCCM
023200020204     C                   ENDIF
023300040301     C                   ENDIF
023400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023600020204     C                   EXSR      CHKNUM
023700020204     C                   IF        PiInt=*on
023800020204     C                   Z-ADD     PiVal         VABLNP
023900020204     C                   ENDIF
024000040301     C                   ENDIF
024100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500020204     C                   Z-ADD     PiVal         VABCTR
024600040301     C                   ENDIF
024700020204     C                   ENDIF
024800020204     C                   ENDIF
024900020204     C*
025000010330     C                   ENDSR
025100010607     C*----------------------------------------------------*
025200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025300010607     C*----------------------------------------------------*
025400010607     C     IMPVAB        BEGSR
025500010607     C*
025600010607     C                   SETOFF                                       3132
025700010607     C*
025800010607     C* Reperimento campi ALFA
025900010607     C*
026000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026100060112     C                   EVAL      VABRSD=%trim(%subst(vindta:51:30))
026200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026400020117     C* ==
026500060112     C                   EVAL      VABIND=%trim(%subst(vindta:81:30))
026600060112     C                   EVAL      VABLOD=%trim(%subst(vindta:111:30))
026700060112     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
026800060112     C                   EVAL      VABRMA=%trim(%subst(vindta:30:10))
026900060112     C                   IF        %trim(%subst(vindta:18:1)) = '1'
027000060112     C                   EVAL      VABCBO='1'
027100060112     C                   ENDIF
027200060112     C                   IF        %trim(%subst(vindta:18:1)) = '6'
027300060112     C                   EVAL      VABCBO='2'
027400060112     C                   ENDIF
027500010607     C*
027600010607     C* Reperimento campi NUMERICI
027700020722     C                   MOVEL     DATCOR        VABAAS
027800020722     C                   MOVE      DATCOR        VABMGS
027900151026     C*
028000151026     C* CTR
028100151026     C* se diverso da blank testo il dato, altrimenti va bene il default
028200151026     C                   IF        %subst(vindta:1:3) <> *blank
028300151026     C                   EVAL      PiStr=%trim(%subst(vindta:1:3))
028400151026     C                   EXSR      CHKNUM
028500151026     C                   IF        PiInt=*on
028600151026     C                   Z-ADD     PiVal         VABCTR
028700151026     C                   ELSE
028800151026     C                   SETON                                        32
028900151026     C                   EVAL      vinmsg = %trimr(vinmsg)
029000151026     C                             + ' ' + 'VABCTR'
029100151026     C                   ENDIF
029200151026     C                   ENDIF
029300060105     C* RMN/NSP
029400060112     C                   EVAL      PiStr=%trim(%subst(vindta:30:10))
029500050408     C                   EXSR      CHKNUM
029600050408     C                   IF        PiInt=*on
029700050408     C                   Z-ADD     PiVal         VABRMN
029800050908     C                   Z-ADD     PiVal         VABNSP
029900050408     C                   ELSE
030000051219     C                   SETON                                        31
030100050408     C                   Z-ADD     *zeros        VABRMN
030200050408     C                   EVAL      vinmsg = %trimr(vinmsg)
030300050908     C                             + ' ' + 'VABRMN VABNSP'
030400050408     C                   ENDIF
030500040420     C* CAD
030600060112     C                   EVAL      PiStr=%trim(%subst(vindta:143:5))
030700010607     C                   EXSR      CHKNUM
030800010607     C                   IF        PiInt=*on
030900020204     C                   Z-ADD     PiVal         Num5_0
031000020204     C                   MOVEL(p)  Num5_0        VABCAD
031100010607     C                   ELSE
031200010607     C                   SETON                                        32
031300020204     C                   EVAL      VABCAD = *zeros
031400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031500020204     C                             + ' ' + 'VABCAD'
031600010607     C                   ENDIF
031700040420     C* NCL
031800060112     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
031900010607     C                   EXSR      CHKNUM
032000010607     C                   IF        PiInt=*on
032100010607     C                   Z-ADD     PiVal         VABNCL
032200010607     C                   ELSE
032300010607     C                   SETON                                        32
032400010607     C                   Z-ADD     *zeros        VABNCL
032500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032600010607     C                             + ' ' + 'VABNCL'
032700010607     C                   ENDIF
032800040420     C* PKB
032900060112     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
033000010607     C                   EXSR      CHKNUM
033100010607     C                   IF        PiNum=*on
033200060112     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
033300050330     C                   Z-ADD(H)  PiVal         VABPKB
033400010607     C                   ELSE
033500010607     C                   SETON                                        32
033600010607     C                   Z-ADD     *zeros        VABPKB
033700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033800010607     C                             + ' ' + 'VABPKB'
033900010607     C                   ENDIF
034000020322     C*
034100020322     C* Considerazioni finali su CBO/CAS
034200050908     C                   IF        FlgCAS = '1'
034300040713     C                   IF        VABCBO = '1'
034400020322     C                   EVAL      VABCBO = '4'
034500040713     C                   ENDIF
034600040713     C                   IF        VABCBO = '2'
034700040713     C                   EVAL      VABCBO = '6'
034800040713     C                   ENDIF
034900020322     C                   ENDIF
035000020204     C*
035100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035200020204     C                   EXSR      CHKIMPDIV
035300020204     C*
035400010607     C                   ENDSR
035500010607     C*----------------------------------------------------*
035600010601
035700020204
035800020204     C*----------------------------------------------------*
035900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
036000020204     C*----------------------------------------------------*
036100020204     C     CHKIMPDIV     BEGSR
036200020204     C*
036300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
036400020204     C                   Z-ADD     *zeros        wrkDec            9 9
036500020204     C*
036600020204     C* Come prima cosa effettuo considerazioni sulla divisa
036700020204     C                   IF        vabIAS > *zeros
036800020204     C                   IF        vabVAS <> 'EUR'
036900020204     C                   EVAL      vabVAS =  'ITL'
037000020204     C                   ENDIF
037100020204     C                   ENDIF
037200020204     C*
037300020204     C                   IF        vabCAS > *zeros
037400020204     C                   IF        vabVCA <> 'EUR'
037500020204     C                   EVAL      vabVCA =  'ITL'
037600020204     C                   ENDIF
037700020204     C                   ENDIF
037800020204     C*
037900020204     C                   IF        vabVMD > *zeros
038000020321     C                   IF        vabVAD <> 'EUR'
038100020204     C                   EVAL      vabVAD =  'ITL'
038200020204     C                   ENDIF
038300020204     C                   ENDIF
038400020204     C*
038500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
038600020204     C                   Z-ADD     vabIAS        wrkDec
038700020204     C                   IF        wrkDec > *zeros
038800020204     C                   IF        vabVAS = 'ITL'
038900020204     C                   EVAL      vabIAS = *zeros
039000020204     C                   ENDIF
039100020204     C                   ENDIF
039200020204     C*
039300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
039400020204     C                   Z-ADD     vabCAS        wrkDec
039500020204     C                   IF        wrkDec > *zeros
039600020204     C                   IF        vabVCA = 'ITL'
039700020204     C                   EVAL      vabCAS = *zeros
039800020204     C                   ENDIF
039900020204     C                   ENDIF
040000020204     C*
040100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
040200020204     C                   Z-ADD     vabVMD        wrkDec
040300020204     C                   IF        wrkDec > *zeros
040400020204     C                   IF        vabVAD = 'ITL'
040500020204     C                   EVAL      vabVMD = *zeros
040600020204     C                   ENDIF
040700020204     C                   ENDIF
040800020204     C*
040900020204     C                   ENDSR
041000020204     C***
041100020204
041200010330
041300010330
041400010330     C*----------------------------------------------------*
041500010330     C*  CONTROLLO NUMERICITA' CAMPI
041600010330     C*----------------------------------------------------*
041700010330     C     CHKNUM        BEGSR
041800010330     C*
041900010606     C                   IF        PiDecChr = *blanks
042000010606     C                   EVAL      PiDecChr = ','
042100010606     C                   ENDIF
042200010606     C*
042300010606     C                   CALL(e)   'ISNUMERIC'
042400010330     C                   PARM                    PiStr            30
042500010606     C                   PARM                    PiDecChr          1
042600010330     C                   PARM      *ZEROS        PiVal            30 9
042700010330     C                   PARM      '0'           PiInt             1
042800010330     C                   PARM      '0'           PiNum             1
042900010330     C                   IF        %error
043000010606     C                   EVAL      PiNum=*off
043100010330     C                   ENDIF
043200010330     C*
043300010330     C                   ENDSR
043400010330     C***
043500010330
043600010601
043700010601
043800010601
043900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044000010601     C     repfil        BEGSR
044100010601     C*
044200010601     C                   if        invfil = *zeros and
044300010601     C                             depfil > *zeros and
044400010629     C                             (vinflg = *blanks or
044500010629     C                              vinflg = *zeros)
044600010601     C
044700010601     C                   eval      invfil = depfil
044800010601     C                   endif
044900010601     C*
045000010601     C                   if        depfil <> invfil and
045100010601     C                             invfil > *zeros
045200010601     C                   eval      flgMulti = '1'
045300010601     C                   if        vinflg = *blanks
045400010601     C                   add       1             cntNonEl
045500010601     C                   endif
045600010601     C                   endif
045700010601     C*
045800010601     C                   if        vinflg = '2'
045900010601     C                   eval      flgStato = '2'
046000010601     C                   endif
046100010601     C*
046200010601     C                   ENDSR
046300010601     C***
046400010601
046500010601
046600010601
046700010330
046800010330
046900010330
047000990920      /TITLE Invio dei dati al punto operativo.
047100000613     C     invio         BEGSR
047200990920     C*
047300990920     C                   reset                   dscmz
047400010601     C                   move      invfil        cmzdst
047500990920     C                   eval      cmzfld = vlrfou
047600990920     C                   eval      cmzmbd = vlrhdl
047700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
047800000710     C                   if        prmfir = *blanks
047900050408     C                   eval      cmzfla = 'FIVAB00F'
048000050408     C                   eval      cmzmba = 'FIVAB00F'
048100000710     C                   else
048200000710     C                   eval      cmzfla = prmfir
048300000710     C                   eval      cmzmba = prmfir
048400000710     C                   endif
048500990920     C                   eval      cmznrr = *zeros
048600990920     C                   move      §ctrok        cmznrr
048700021018     C                   eval      cmzlba = vlrfl1
048800990920     C                   call(e)   'TIS711C'
048900990920     C                   parm                    dscmz
049000990921     C                   parm      *blanks       esito
049100990920     C                   if        %error
049200990920     C                             or cmzerr = '1'
049300990921     C                             or esito  = '1'
049400000710     C                   eval      wrkesito = '3'
049500990920     C                   endif
049600990920     C*
049700000613     C                   ENDSR
049800990910
049900010601
050000010601
050100010601
050200010601
050300010601      /TITLE Invio dei dati al punto operativo.
050400010601     C     opeini        BEGSR
050500010601     C*
050600010601     C* Inizializzo flag e contatori operativi
050700010601     C                   movel     '0'           flgGiro           1
050800010601     C                   movel     '0'           flgMulti          1
050900010601     C                   movel     '1'           flgStato          1
051000010615     C                   movel     '0'           flgOk             1
051100010601     C                   z-add     *zeros        cntNonEl         10 0
051200010601     C                   z-add     *zeros        depfil            3 0
051300010601     C                   z-add     *zeros        invfil            3 0
051400010601     C*
051500010601     C                   ENDSR
051600010601     C***
051700010601
051800010601
051900010601
052000010330
052100010330
052200000613     C     *inzsr        BEGSR
052300990910     C*
052400990910     C     *entry        plist
052500990920     C                   parm                    tivlrds
052600990921     C                   parm      wrkesito      esito
052700000724     C                   parm                    prmlit
052800000710     C                   parm                    prmfir
052900010330     C*
053000010330     C* CALCOLA LA DATA CORRENTE
053100010330     C                   time                    wn14             14 0
053200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053300010330     C                   z-add     wn8           g08dat
053400010330     C                   z-add     *zeros        g08inv
053500010330     C                   movel     '0'           g08err
053600010330     C                   call      'XSRDA8'
053700010330     C                   parm                    wlbda8
053800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
053900000613     C*
054000000613     C                   ENDSR
054100000613     C***
