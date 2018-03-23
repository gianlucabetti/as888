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
022000060105     C                   EVAL      VABCCM = 2020790
022100060105     C                   EVAL      VABLNP = 202
022200060105     C                   EVAL      VABCTR = 000
022300060105     C                   EVAL      VABTSP = 'D'
022400020204     C* ... e poi verifico se sono stati passati come parametri
022500020204     C                   IF        vlrppt > *blanks
022600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022800020204     C                   EXSR      CHKNUM
022900020204     C                   IF        PiInt=*on
023000020204     C                   Z-ADD     PiVal         VABCCM
023100020204     C                   ENDIF
023200040301     C                   ENDIF
023300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023500020204     C                   EXSR      CHKNUM
023600020204     C                   IF        PiInt=*on
023700020204     C                   Z-ADD     PiVal         VABLNP
023800020204     C                   ENDIF
023900040301     C                   ENDIF
024000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024200020204     C                   EXSR      CHKNUM
024300020204     C                   IF        PiInt=*on
024400020204     C                   Z-ADD     PiVal         VABCTR
024500040301     C                   ENDIF
024600020204     C                   ENDIF
024700020204     C                   ENDIF
024800020204     C*
024900010330     C                   ENDSR
025000010607     C*----------------------------------------------------*
025100050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025200010607     C*----------------------------------------------------*
025300010607     C     IMPVAB        BEGSR
025400010607     C*
025500010607     C                   SETOFF                                       3132
025600010607     C*
025700010607     C* Reperimento campi ALFA
025800010607     C*
025900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026000060105     C                   EVAL      VABRSD=%trim(%subst(vindta:3:35))
026100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026300020117     C* ==
026400060105     C                   EVAL      VABRD2=%trim(%subst(vindta:38:35))
026500060105     C                   EVAL      VABIND=%trim(%subst(vindta:73:35))
026600060105     C                   EVAL      VABLOD=%trim(%subst(vindta:117:35))
026700060105     C                   EVAL      VABPRD=%trim(%subst(vindta:152:2))
026800060105     C                   EVAL      VABRMA=%trim(%subst(vindta:192:8))
026900060105     C                   EVAL      VABCBO=%trim(%subst(vindta:2:1))
027000060105     C                   EVAL      VABTIC=%trim(%subst(vindta:183:2))
027100060105     C                   EVAL      VABNOT=%trim(%subst(vindta:200:35))
027200071116     C                   EVAL      VABNT2=%trim(%subst(vindta:235:35))
027300010607     C*
027400010607     C* Reperimento campi NUMERICI
027500020722     C                   MOVEL     DATCOR        VABAAS
027600020722     C                   MOVE      DATCOR        VABMGS
027700060105     C* RMN/NSP
027800060105     C                   EVAL      PiStr=%trim(%subst(vindta:192:8))
027900050408     C                   EXSR      CHKNUM
028000050408     C                   IF        PiInt=*on
028100050408     C                   Z-ADD     PiVal         VABRMN
028200050908     C                   Z-ADD     PiVal         VABNSP
028300050408     C                   ELSE
028400051219     C                   SETON                                        31
028500050408     C                   Z-ADD     *zeros        VABRMN
028600050408     C                   EVAL      vinmsg = %trimr(vinmsg)
028700050908     C                             + ' ' + 'VABRMN VABNSP'
028800050408     C                   ENDIF
028900040420     C* CAD
029000060105     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
029100010607     C                   EXSR      CHKNUM
029200010607     C                   IF        PiInt=*on
029300020204     C                   Z-ADD     PiVal         Num5_0
029400020204     C                   MOVEL(p)  Num5_0        VABCAD
029500010607     C                   ELSE
029600010607     C                   SETON                                        32
029700020204     C                   EVAL      VABCAD = *zeros
029800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
029900020204     C                             + ' ' + 'VABCAD'
030000010607     C                   ENDIF
030100040420     C* NCL
030200060105     C                   EVAL      PiStr=%trim(%subst(vindta:154:5))
030300010607     C                   EXSR      CHKNUM
030400010607     C                   IF        PiInt=*on
030500010607     C                   Z-ADD     PiVal         VABNCL
030600010607     C                   ELSE
030700010607     C                   SETON                                        32
030800010607     C                   Z-ADD     *zeros        VABNCL
030900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031000010607     C                             + ' ' + 'VABNCL'
031100010607     C                   ENDIF
031200040420     C* PKB
031300060105     C                   EVAL      PiStr=%trim(%subst(vindta:159:9))
031400010607     C                   EXSR      CHKNUM
031500010607     C                   IF        PiNum=*on
031600050330     C                   Z-ADD(H)  PiVal         VABPKB
031700010607     C                   ELSE
031800010607     C                   SETON                                        32
031900010607     C                   Z-ADD     *zeros        VABPKB
032000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032100010607     C                             + ' ' + 'VABPKB'
032200010607     C                   ENDIF
032300051219     C* CAS
032400060105     C                   IF        %trim(%subst(vindta:173:10))<>*zeros
032500060105     C                   EVAL      PiStr=%trim(%subst(vindta:173:10))
032600060105     C                   EVAL      VABVCA='EUR'
032700051219     C                   EVAL      FlgCAS = '1'
032800051219     C                   EXSR      CHKNUM
032900051219     C                   IF        PiNum=*on
033000060105     C                   Z-ADD     PiVal         VABCAS
033100051219     C                   ELSE
033200051219     C                   SETON                                        32
033300051219     C                   Z-ADD     *zeros        VABCAS
033400051219     C                   EVAL      vinmsg = %trimr(vinmsg)
033500051219     C                             + ' ' + 'VABCAS'
033600051219     C                   ENDIF
033700051219     C                   ENDIF
033800020322     C*
033900020322     C* Considerazioni finali su CBO/CAS
034000050908     C                   IF        FlgCAS = '1'
034100040713     C                   IF        VABCBO = '1'
034200020322     C                   EVAL      VABCBO = '4'
034300040713     C                   ENDIF
034400040713     C                   IF        VABCBO = '2'
034500040713     C                   EVAL      VABCBO = '6'
034600040713     C                   ENDIF
034700020322     C                   ENDIF
034800020204     C*
034900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035000020204     C                   EXSR      CHKIMPDIV
035100020204     C*
035200010607     C                   ENDSR
035300010607     C*----------------------------------------------------*
035400010601
035500020204
035600020204     C*----------------------------------------------------*
035700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
035800020204     C*----------------------------------------------------*
035900020204     C     CHKIMPDIV     BEGSR
036000020204     C*
036100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
036200020204     C                   Z-ADD     *zeros        wrkDec            9 9
036300020204     C*
036400020204     C* Come prima cosa effettuo considerazioni sulla divisa
036500020204     C                   IF        vabIAS > *zeros
036600020204     C                   IF        vabVAS <> 'EUR'
036700020204     C                   EVAL      vabVAS =  'ITL'
036800020204     C                   ENDIF
036900020204     C                   ENDIF
037000020204     C*
037100020204     C                   IF        vabCAS > *zeros
037200020204     C                   IF        vabVCA <> 'EUR'
037300020204     C                   EVAL      vabVCA =  'ITL'
037400020204     C                   ENDIF
037500020204     C                   ENDIF
037600020204     C*
037700020204     C                   IF        vabVMD > *zeros
037800020321     C                   IF        vabVAD <> 'EUR'
037900020204     C                   EVAL      vabVAD =  'ITL'
038000020204     C                   ENDIF
038100020204     C                   ENDIF
038200020204     C*
038300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
038400020204     C                   Z-ADD     vabIAS        wrkDec
038500020204     C                   IF        wrkDec > *zeros
038600020204     C                   IF        vabVAS = 'ITL'
038700020204     C                   EVAL      vabIAS = *zeros
038800020204     C                   ENDIF
038900020204     C                   ENDIF
039000020204     C*
039100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
039200020204     C                   Z-ADD     vabCAS        wrkDec
039300020204     C                   IF        wrkDec > *zeros
039400020204     C                   IF        vabVCA = 'ITL'
039500020204     C                   EVAL      vabCAS = *zeros
039600020204     C                   ENDIF
039700020204     C                   ENDIF
039800020204     C*
039900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
040000020204     C                   Z-ADD     vabVMD        wrkDec
040100020204     C                   IF        wrkDec > *zeros
040200020204     C                   IF        vabVAD = 'ITL'
040300020204     C                   EVAL      vabVMD = *zeros
040400020204     C                   ENDIF
040500020204     C                   ENDIF
040600020204     C*
040700020204     C                   ENDSR
040800020204     C***
040900020204
041000010330
041100010330
041200010330     C*----------------------------------------------------*
041300010330     C*  CONTROLLO NUMERICITA' CAMPI
041400010330     C*----------------------------------------------------*
041500010330     C     CHKNUM        BEGSR
041600010330     C*
041700010606     C                   IF        PiDecChr = *blanks
041800010606     C                   EVAL      PiDecChr = ','
041900010606     C                   ENDIF
042000010606     C*
042100010606     C                   CALL(e)   'ISNUMERIC'
042200010330     C                   PARM                    PiStr            30
042300010606     C                   PARM                    PiDecChr          1
042400010330     C                   PARM      *ZEROS        PiVal            30 9
042500010330     C                   PARM      '0'           PiInt             1
042600010330     C                   PARM      '0'           PiNum             1
042700010330     C                   IF        %error
042800010606     C                   EVAL      PiNum=*off
042900010330     C                   ENDIF
043000010330     C*
043100010330     C                   ENDSR
043200010330     C***
043300010330
043400010601
043500010601
043600010601
043700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043800010601     C     repfil        BEGSR
043900010601     C*
044000010601     C                   if        invfil = *zeros and
044100010601     C                             depfil > *zeros and
044200010629     C                             (vinflg = *blanks or
044300010629     C                              vinflg = *zeros)
044400010601     C
044500010601     C                   eval      invfil = depfil
044600010601     C                   endif
044700010601     C*
044800010601     C                   if        depfil <> invfil and
044900010601     C                             invfil > *zeros
045000010601     C                   eval      flgMulti = '1'
045100010601     C                   if        vinflg = *blanks
045200010601     C                   add       1             cntNonEl
045300010601     C                   endif
045400010601     C                   endif
045500010601     C*
045600010601     C                   if        vinflg = '2'
045700010601     C                   eval      flgStato = '2'
045800010601     C                   endif
045900010601     C*
046000010601     C                   ENDSR
046100010601     C***
046200010601
046300010601
046400010601
046500010330
046600010330
046700010330
046800990920      /TITLE Invio dei dati al punto operativo.
046900000613     C     invio         BEGSR
047000990920     C*
047100990920     C                   reset                   dscmz
047200010601     C                   move      invfil        cmzdst
047300990920     C                   eval      cmzfld = vlrfou
047400990920     C                   eval      cmzmbd = vlrhdl
047500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
047600000710     C                   if        prmfir = *blanks
047700050408     C                   eval      cmzfla = 'FIVAB00F'
047800050408     C                   eval      cmzmba = 'FIVAB00F'
047900000710     C                   else
048000000710     C                   eval      cmzfla = prmfir
048100000710     C                   eval      cmzmba = prmfir
048200000710     C                   endif
048300990920     C                   eval      cmznrr = *zeros
048400990920     C                   move      §ctrok        cmznrr
048500021018     C                   eval      cmzlba = vlrfl1
048600990920     C                   call(e)   'TIS711C'
048700990920     C                   parm                    dscmz
048800990921     C                   parm      *blanks       esito
048900990920     C                   if        %error
049000990920     C                             or cmzerr = '1'
049100990921     C                             or esito  = '1'
049200000710     C                   eval      wrkesito = '3'
049300990920     C                   endif
049400990920     C*
049500000613     C                   ENDSR
049600990910
049700010601
049800010601
049900010601
050000010601
050100010601      /TITLE Invio dei dati al punto operativo.
050200010601     C     opeini        BEGSR
050300010601     C*
050400010601     C* Inizializzo flag e contatori operativi
050500010601     C                   movel     '0'           flgGiro           1
050600010601     C                   movel     '0'           flgMulti          1
050700010601     C                   movel     '1'           flgStato          1
050800010615     C                   movel     '0'           flgOk             1
050900010601     C                   z-add     *zeros        cntNonEl         10 0
051000010601     C                   z-add     *zeros        depfil            3 0
051100010601     C                   z-add     *zeros        invfil            3 0
051200010601     C*
051300010601     C                   ENDSR
051400010601     C***
051500010601
051600010601
051700010601
051800010330
051900010330
052000000613     C     *inzsr        BEGSR
052100990910     C*
052200990910     C     *entry        plist
052300990920     C                   parm                    tivlrds
052400990921     C                   parm      wrkesito      esito
052500000724     C                   parm                    prmlit
052600000710     C                   parm                    prmfir
052700010330     C*
052800010330     C* CALCOLA LA DATA CORRENTE
052900010330     C                   time                    wn14             14 0
053000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053100010330     C                   z-add     wn8           g08dat
053200010330     C                   z-add     *zeros        g08inv
053300010330     C                   movel     '0'           g08err
053400010330     C                   call      'XSRDA8'
053500010330     C                   parm                    wlbda8
053600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
053700000613     C*
053800000613     C                   ENDSR
053900000613     C***
