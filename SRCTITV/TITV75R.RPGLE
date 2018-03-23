000100031117      /TITLE Upload via Internet: traduzione in FIVABWWR. (x GLOBUS - Cod. 2020947)
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
022500100108     C                   EVAL      VABCCM = 2022648
022600031117     C                   EVAL      VABLNP = 202
022700031013     C                   EVAL      VABCTR = 000
022800031117     C                   EVAL      VABTSP = 'D'
022900020204     C* ... e poi verifico se sono stati passati come parametri
023000020204     C                   IF        vlrppt > *blanks
023100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023200020204     C                   EXSR      CHKNUM
023300020204     C                   IF        PiInt=*on
023400020204     C                   Z-ADD     PiVal         VABCCM
023500020204     C                   ENDIF
023600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023700020204     C                   EXSR      CHKNUM
023800020204     C                   IF        PiInt=*on
023900020204     C                   Z-ADD     PiVal         VABLNP
024000020204     C                   ENDIF
024100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024200020204     C                   EXSR      CHKNUM
024300020204     C                   IF        PiInt=*on
024400020204     C                   Z-ADD     PiVal         VABCTR
024500020204     C                   ENDIF
024600020204     C                   ENDIF
024700020204     C*
024800010330     C                   ENDSR
024900010607     C*----------------------------------------------------*
025000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025100010607     C*----------------------------------------------------*
025200010607     C     IMPVAB        BEGSR
025300010607     C*
025400010607     C                   SETOFF                                       3132
025500030515     C*
025600010607     C                   MOVEL     datcor        VABAAS
025700010607     C                   MOVE      datcor        VABMGS
025800010607     C*
025900010607     C* Reperimento campi ALFA
026000010607     C*
026100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026200031117     C                   EVAL      VABRSD=%trim(%subst(vindta:1:50))
026300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026500020117     C* ==
026600031117     C                   EVAL      VABIND=%trim(%subst(vindta:51:25))
026700031117     C                   EVAL      VABLOD=%trim(%subst(vindta:81:25))
026800031117     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
026900031117     C                   EVAL      VABRMA=%trim(%subst(vindta:120:6))
027000010607     C*
027100010607     C* Reperimento campi NUMERICI
027200020604     C*
027300031029     C* CAD
027400031117     C                   EVAL      PiStr=%trim(%subst(vindta:76:5))
027500010607     C                   EXSR      CHKNUM
027600010607     C                   IF        PiInt=*on
027700020204     C                   Z-ADD     PiVal         Num5_0
027800020204     C                   MOVEL(p)  Num5_0        VABCAD
027900010607     C                   ELSE
028000010607     C                   SETON                                        32
028100020204     C                   EVAL      VABCAD = *zeros
028200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
028300020204     C                             + ' ' + 'VABCAD'
028400010607     C                   ENDIF
028500030515     C* RMN/NSP
028600031117     C                   EVAL      PiStr=%trim(%subst(vindta:120:6))
028700011023     C                   EXSR      CHKNUM
028800011023     C                   IF        PiInt=*on
028900030225     C                   Z-ADD     PiVal         VABRMN
029000031013     C                   MOVE(P)   VABRMN        VABNSP
029100011023     C                   ELSE
029200020205     C                   EVAL      VABRMN = 1
029300011023     C                   ENDIF
029400030515     C* NCL
029500031117     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
029600010607     C                   EXSR      CHKNUM
029700010607     C                   IF        PiInt=*on
029800010607     C                   Z-ADD     PiVal         VABNCL
029900010607     C                   ELSE
030000010607     C                   SETON                                        32
030100010607     C                   Z-ADD     *zeros        VABNCL
030200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
030300010607     C                             + ' ' + 'VABNCL'
030400010607     C                   ENDIF
030500031013     C* PKB
030600031117     C                   EVAL      PiStr=%trim(%subst(vindta:113:6))
030700031013     C                   EXSR      CHKNUM
030800031017     C                   IF        PiNum=*on
030900031117     C                   EVAL      VABPKB = PiVal/10                            * gestisco 1 decim.
031000031013     C                   ELSE
031100031013     C                   SETON                                        32
031200031013     C                   Z-ADD     *zeros        VABPKB
031300031013     C                   EVAL      vinmsg = %trimr(vinmsg)
031400031013     C                             + ' ' + 'VABPKB'
031500031013     C                   ENDIF
031600030929     C*
031700030929     C* Considerazioni finali su CBO/CAS
031800030929     C                   IF        FlgCAS = '1'
031900030929     C                   EVAL      VABCBO = '4'
032000030929     C                   ELSE
032100030929     C                   EVAL      VABCBO = '1'
032200030929     C                   ENDIF
032300020204     C*
032400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
032500020204     C                   EXSR      CHKIMPDIV
032600020204     C*
032700010607     C                   ENDSR
032800010607     C*----------------------------------------------------*
032900010601
033000020204
033100020204     C*----------------------------------------------------*
033200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
033300020204     C*----------------------------------------------------*
033400020204     C     CHKIMPDIV     BEGSR
033500020204     C*
033600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
033700020204     C                   Z-ADD     *zeros        wrkDec            9 9
033800020204     C*
033900020204     C* Come prima cosa effettuo considerazioni sulla divisa
034000020204     C                   IF        vabIAS > *zeros
034100020204     C                   IF        vabVAS <> 'EUR'
034200020204     C                   EVAL      vabVAS =  'ITL'
034300020204     C                   ENDIF
034400020204     C                   ENDIF
034500020204     C*
034600020204     C                   IF        vabCAS > *zeros
034700020204     C                   IF        vabVCA <> 'EUR'
034800020204     C                   EVAL      vabVCA =  'ITL'
034900020204     C                   ENDIF
035000020204     C                   ENDIF
035100020204     C*
035200020204     C                   IF        vabVMD > *zeros
035300020204     C                   IF        vabVAD <> 'EUR'
035400020204     C                   EVAL      vabVAD =  'ITL'
035500020204     C                   ENDIF
035600020204     C                   ENDIF
035700020204     C*
035800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
035900020204     C                   Z-ADD     vabIAS        wrkDec
036000020204     C                   IF        wrkDec > *zeros
036100020204     C                   IF        vabVAS = 'ITL'
036200020204     C                   EVAL      vabIAS = *zeros
036300020204     C                   ENDIF
036400020204     C                   ENDIF
036500020204     C*
036600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
036700020204     C                   Z-ADD     vabCAS        wrkDec
036800020204     C                   IF        wrkDec > *zeros
036900020204     C                   IF        vabVCA = 'ITL'
037000020204     C                   EVAL      vabCAS = *zeros
037100020204     C                   ENDIF
037200020204     C                   ENDIF
037300020204     C*
037400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
037500020204     C                   Z-ADD     vabVMD        wrkDec
037600020204     C                   IF        wrkDec > *zeros
037700020204     C                   IF        vabVAD = 'ITL'
037800020204     C                   EVAL      vabVMD = *zeros
037900020204     C                   ENDIF
038000020204     C                   ENDIF
038100020204     C*
038200020204     C                   ENDSR
038300020204     C***
038400020204
038500010330
038600010330
038700010330     C*----------------------------------------------------*
038800010330     C*  CONTROLLO NUMERICITA' CAMPI
038900010330     C*----------------------------------------------------*
039000010330     C     CHKNUM        BEGSR
039100010330     C*
039200010606     C                   IF        PiDecChr = *blanks
039300030606     C                   EVAL      PiDecChr = ','
039400010606     C                   ENDIF
039500010606     C*
039600010606     C                   CALL(e)   'ISNUMERIC'
039700010330     C                   PARM                    PiStr            30
039800010606     C                   PARM                    PiDecChr          1
039900010330     C                   PARM      *ZEROS        PiVal            30 9
040000010330     C                   PARM      '0'           PiInt             1
040100010330     C                   PARM      '0'           PiNum             1
040200010330     C                   IF        %error
040300010606     C                   EVAL      PiNum=*off
040400010330     C                   ENDIF
040500010330     C*
040600010330     C                   ENDSR
040700010330     C***
040800010330
040900010601
041000010601
041100010601
041200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
041300010601     C     repfil        BEGSR
041400010601     C*
041500010601     C                   if        invfil = *zeros and
041600010601     C                             depfil > *zeros and
041700010629     C                             (vinflg = *blanks or
041800010629     C                              vinflg = *zeros)
041900010601     C
042000010601     C                   eval      invfil = depfil
042100010601     C                   endif
042200010601     C*
042300010601     C                   if        depfil <> invfil and
042400010601     C                             invfil > *zeros
042500010601     C                   eval      flgMulti = '1'
042600010601     C                   if        vinflg = *blanks
042700010601     C                   add       1             cntNonEl
042800010601     C                   endif
042900010601     C                   endif
043000010601     C*
043100010601     C                   if        vinflg = '2'
043200010601     C                   eval      flgStato = '2'
043300010601     C                   endif
043400010601     C*
043500010601     C                   ENDSR
043600010601     C***
043700010601
043800010601
043900010601
044000010330
044100010330
044200010330
044300990920      /TITLE Invio dei dati al punto operativo.
044400000613     C     invio         BEGSR
044500990920     C*
044600990920     C                   reset                   dscmz
044700010601     C                   move      invfil        cmzdst
044800990920     C                   eval      cmzfld = vlrfou
044900990920     C                   eval      cmzmbd = vlrhdl
045000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
045100000710     C                   if        prmfir = *blanks
045200021025     C                   eval      cmzfla = 'FIVAB00F'
045300021025     C                   eval      cmzmba = 'FIVAB00F'
045400000710     C                   else
045500000710     C                   eval      cmzfla = prmfir
045600000710     C                   eval      cmzmba = prmfir
045700000710     C                   endif
045800990920     C                   eval      cmznrr = *zeros
045900990920     C                   move      §ctrok        cmznrr
046000021018     C                   eval      cmzlba = vlrfl1
046100990920     C                   call(e)   'TIS711C'
046200990920     C                   parm                    dscmz
046300990921     C                   parm      *blanks       esito
046400990920     C                   if        %error
046500990920     C                             or cmzerr = '1'
046600990921     C                             or esito  = '1'
046700000710     C                   eval      wrkesito = '3'
046800990920     C                   endif
046900990920     C*
047000000613     C                   ENDSR
047100990910
047200010601
047300010601
047400010601
047500010601
047600010601      /TITLE Invio dei dati al punto operativo.
047700010601     C     opeini        BEGSR
047800010601     C*
047900010601     C* Inizializzo flag e contatori operativi
048000010601     C                   movel     '0'           flgGiro           1
048100010601     C                   movel     '0'           flgMulti          1
048200010601     C                   movel     '1'           flgStato          1
048300010615     C                   movel     '0'           flgOk             1
048400010601     C                   z-add     *zeros        cntNonEl         10 0
048500010601     C                   z-add     *zeros        depfil            3 0
048600010601     C                   z-add     *zeros        invfil            3 0
048700010601     C*
048800010601     C                   ENDSR
048900010601     C***
049000010601
049100010601
049200010601
049300010330
049400010330
049500000613     C     *inzsr        BEGSR
049600990910     C*
049700990910     C     *entry        plist
049800990920     C                   parm                    tivlrds
049900990921     C                   parm      wrkesito      esito
050000000724     C                   parm                    prmlit
050100000710     C                   parm                    prmfir
050200010330     C*
050300010330     C* CALCOLA LA DATA CORRENTE
050400010330     C                   time                    wn14             14 0
050500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
050600010330     C                   z-add     wn8           g08dat
050700010330     C                   z-add     *zeros        g08inv
050800010330     C                   movel     '0'           g08err
050900010330     C                   call      'XSRDA8'
051000010330     C                   parm                    wlbda8
051100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051200000613     C*
051300000613     C                   ENDSR
051400000613     C***
