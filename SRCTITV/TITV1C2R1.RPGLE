000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
000600140129     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500070502     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100140129     D parccm          s              8    INZ(*blanks)
002200140129     D parmbr          s             10    INZ(*blanks)
002300140129     D paropz          s              1    INZ(*blanks)
002400140129     D chkcall         s              1    INZ(*blanks)
002500140129     D w40             s             40
002600010330     D*------------------
002700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010330     D*------------------
002900010330     D WLBDA8          DS                  INZ
003000010330     D  G08DAT                 1      8  0
003100010330     D  G08INV                 9     16  0
003200010330     D  G08ERR                17     17
003300010330     D  G08TGI                18     22  0
003400010330
003500010330
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010601     C*
004000010601     C                   exsr      opeini
004100010605     C                   exsr      rwvab
004200070502     C*
004300070502     C* Effettuo lancio TISI95 solo x chiusura
004400070502     C                   CLEAR                   TISI95DS
004500070502     C                   EVAL      I95TLA = 'C'
004600070502     C                   CALL      'TISI95R'
004700070502     C                   PARM                    TISI95DS
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
008000140129
008100010330     C*--------------------------------------------------------
008200050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
008300010330     C*--------------------------------------------------------
008400010605     C     RWVAB         BEGSR
008500050408     C*
008600010330     C                   if        not %open(tivin00r)
008700010330     C                   open      tivin00r
008800010330     C                   endif
008900050408     C                   if        not %open(fivabwwr)
009000050408     C                   open      fivabwwr
009100010330     C                   endif
009200010330     C*
009300140129     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009400140129     C                   exsr      prevat
009500140129     C*
009600140129     C                   if        chkcall = '0'
009700140129     C*
009800140129     C                   if        not %open(fivatwwr)
009900140129     C                   open      fivatwwr
010000140129     C                   endif
010100140129     C*
010200010604     C                   clear                   §CTROK
010300010604     C                   clear                   §CTRMO
010400010604     C                   clear                   §CTRNO
010500010330     C*
010600010330     C                   DO        *HIVAL
010700010330     C*
010800010330     C                   READ      tivin00r                               70
010900010618     C*
011000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011100010618     C                   if        vinflg = '1'
011200010618     C                   eval      flgOk = '1'
011300010618     C                   endif
011400010618     C*
011500010330     C                   if        vindta > *blanks
011600010330     C                   add       1             rrnum
011700010330     C*
011800010601     C                   if        *in70 = *off and
011900010330     C                             (vinflg = *blanks
012000010330     C                              or vinflg = '0'
012100010330     C                              or vinflg = '2')
012200140129     C*
012300140129     C                   clear                   FIVAB000
012400140129     C                   clear                   FIVAT000
012500010330     C*
012600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012700010711     C                   if        vinflg = *blanks or vinflg = '0'
012800010711     C                   clear                   vinmsg
012900010711     C                   endif
013000070601     C*
013100070601     C                   exsr      inzvar
013200070601     C                   exsr      defcam
013300070601     C                   exsr      impvab
013400010601     C*
013500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013600010605     C                   eval      depfil = VABLNP
013700010601     C                   exsr      repfil
013800010601     C                   if        depfil = invfil
013900021025     C                   if        vlrpoi = 999
014000021025     C                   MOVE(P)   invfil        VABFGS
014100021025     C                   else
014200021025     C                   MOVE(P)   vlrpoi        VABFGS
014300021025     C                   endif
014400010601     C*
014500010601     C                   exsr      PREELA
014600010601     C*
014700010604     C* Ebbene...
014800010604     C*
014900010604     C  N31              ADD       1             §CTROK            7 0
015000010604     C   32              ADD       1             §CTRMO            7 0
015100010604     C   31              ADD       1             §CTRNO            7 0
015200071003     C                   exsr      wrivab                                       => scrivo VAB
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
019500140129     C*
019600140129     C                   else
019700140129     C                   eval      wrkesito = '2'
019800140129     C                   endif
019900010330     C*
020000010330     C                   if        %open(tivin00r)
020100010330     C                   close     tivin00r
020200010330     C                   endif
020300050408     C                   if        %open(fivabwwr)
020400050408     C                   close     fivabwwr
020500010330     C                   endif
020600140129     C                   if        %open(fivatwwr)
020700140129     C                   close     fivatwwr
020800140129     C                   endif
020900010601     C*
021000010601     C                   if        vlrpoi <> 999
021100010601     C                   eval      invfil = vlrpoi
021200010601     C                   endif
021300010330     C*
021400010330     C                   if        §ctrok > 0
021500010601     C                             and invfil > *zeros
021600010330     C                   exsr      invio
021700010330     C                   endif
021800010601     C*
021900010618     C                   if        flgGiro = '1'
022000010601     C                   exsr      endela
022100010618     C                   endif
022200010330     C*
022300010330     C                   ENDSR
022400010330     C***
022500070601
022600070601     C*----------------------------------------------------*
022700070601     C*  SCARICAMENTO BUFFER RECORDS VAB
022800070601     C*----------------------------------------------------*
022900070601     C     WRIVAB        BEGSR
023000070601     C*
023100070601     C  N31              WRITE     FIVAB000
023200140129     C  N31              EXSR      WRIVAT_E
023300070601     C*
023400070601     C                   ENDSR
023500010601
023600010601
023700010601
023800010330     C*----------------------------------------------------*
023900020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024000010330     C*----------------------------------------------------*
024100010330     C     INZVAR        BEGSR
024200010330     C*
024300020204     C                   Z-ADD     *zeros        Num5_0            5 0
024400020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024500020322     C                   MOVEL     '0'           FlgCAS            1
024600010330     C*
024700010330     C                   ENDSR
024800010330     C*----------------------------------------------------*
024900020722     C*  IMPOSTAZIONE CAMPI COSTANTI
025000010330     C*----------------------------------------------------*
025100010330     C     DEFCAM        BEGSR
025200010330     C*
025300020204     C* Inizializzo il buffer del record da scrivere
025400050408     C                   CLEAR                   FIVAB000
025500020204     C* Imposto i valori di default...
025600071030     C                   EVAL      VABCCM = 0013573
025700071030     C                   EVAL      VABLNP = 001
025800071030     C                   EVAL      VABCTR = 300
025900071022     C                   EVAL      VABCTM = '7Q'
026000070208     C                   EVAL      VABCBO = '1'
026100020204     C* ... e poi verifico se sono stati passati come parametri
026200020204     C                   IF        vlrppt > *blanks
026300040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
026400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026500020204     C                   EXSR      CHKNUM
026600020204     C                   IF        PiInt=*on
026700020204     C                   Z-ADD     PiVal         VABCCM
026800020204     C                   ENDIF
026900040301     C                   ENDIF
027000040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
027100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027200020204     C                   EXSR      CHKNUM
027300020204     C                   IF        PiInt=*on
027400020204     C                   Z-ADD     PiVal         VABLNP
027500020204     C                   ENDIF
027600040301     C                   ENDIF
027700040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
027800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027900020204     C                   EXSR      CHKNUM
028000020204     C                   IF        PiInt=*on
028100020204     C                   Z-ADD     PiVal         VABCTR
028200040301     C                   ENDIF
028300020204     C                   ENDIF
028400020204     C                   ENDIF
028500020204     C*
028600010330     C                   ENDSR
028700010607     C*----------------------------------------------------*
028800050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028900010607     C*----------------------------------------------------*
029000010607     C     IMPVAB        BEGSR
029100010607     C*
029200010607     C                   SETOFF                                       3132
029300010607     C*
029400010607     C* Reperimento campi ALFA
029500010607     C*
029600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
029700071030     C                   EVAL      VABRSD=%trim(%subst(vindta:3:30))
029800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030000020117     C* ==
030100071030     C                   IF        %trim(%subst(vindta:1:2)) = '1'
030200071030     C                   EVAL      VABCBO = '2'
030300071030     C                   ENDIF
030400071030     C                   IF        %trim(%subst(vindta:1:2)) = '2'
030500071030     C                   EVAL      VABCBO = '1'
030600071030     C                   ENDIF
030700140129     C                   EVAL      VABRD2=%trim(%subst(vindta:33:25))
030800140129     C                   EVAL      VABIND=%trim(%subst(vindta:250:30))
030900071030     C                   EVAL      VABLOD=%trim(%subst(vindta:58:30))
031000071030     C                   EVAL      VABPRD=%trim(%subst(vindta:93:2))
031100071030     C                   EVAL      VABRMA=%trim(%subst(vindta:193:15))
031200071030     C                   EVAL      VABNOT=%trim(%subst(vindta:133:35))
031300071030     C                   EVAL      VABNT2=%trim(%subst(vindta:133+35:50-35))
031400060510     C*
031500060510     C* Reperimento campi NUMERICI
031600060510     C                   MOVEL     DATCOR        VABAAS
031700060510     C                   MOVE      DATCOR        VABMGS
031800060710     C* NSP/RMN
031900071030     C                   EVAL      PiStr=%trim(%subst(vindta:115:6))
032000060510     C                   EXSR      CHKNUM
032100060510     C                   IF        PiInt=*on
032200060510     C                   Z-ADD     PiVal         VABNSP
032300060510     C                   Z-ADD     PiVal         VABRMN
032400060404     C                   ELSE
032500070502     C                   SETON                                        31
032600060510     C                   Z-ADD     *zeros        VABNSP
032700060510     C                   Z-ADD     1             VABRMN
032800060404     C                   EVAL      vinmsg = %trimr(vinmsg)
032900060510     C                             + ' ' + 'VABNSP VABRMN'
033000060404     C                   ENDIF
033100040420     C* CAD
033200071030     C                   EVAL      PiStr=%trim(%subst(vindta:88:5))
033300010607     C                   EXSR      CHKNUM
033400010607     C                   IF        PiInt=*on
033500020204     C                   Z-ADD     PiVal         Num5_0
033600020204     C                   MOVEL(p)  Num5_0        VABCAD
033700010607     C                   ELSE
033800010607     C                   SETON                                        32
033900020204     C                   EVAL      VABCAD = *zeros
034000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034100020204     C                             + ' ' + 'VABCAD'
034200060316     C                   ENDIF
034300070502     C* Reperisco la provincia dal CAP e dalla località
034400070502     C                   IF        VABCAD <> *blanks AND
034500070502     C                             VABPRD  = *blanks
034600070502     C                   CLEAR                   TISI95DS
034700070502     C                   EVAL      I95TCN = '3'
034800070502     C                   Z-ADD     datcor        I95DAT
034900070502     C                   EVAL      I95CAP = VABCAD
035000070502     C                   EVAL      I95LOC = VABLOD
035100070502     C                   EVAL      I95NAR = VABNZD
035200070502     C                   CALL      'TISI95R'
035300070502     C                   PARM                    TISI95DS
035400070502     C                   EVAL      VABPRD = O95PRV
035500070502     C                   ENDIF
035600040420     C* NCL
035700071030     C                   EVAL      PiStr=%trim(%subst(vindta:95:4))
035800010607     C                   EXSR      CHKNUM
035900010607     C                   IF        PiInt=*on
036000070606     C                   Z-ADD     PiVal         VABNCL
036100010607     C                   ELSE
036200010607     C                   SETON                                        32
036300010607     C                   Z-ADD     *zeros        VABNCL
036400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036500010607     C                             + ' ' + 'VABNCL'
036600010607     C                   ENDIF
036700071030     C* VLB
036800071030     C                   EVAL      PiStr=%trim(%subst(vindta:99:8))
036900071030     C                   EXSR      CHKNUM
037000071030     C                   IF        PiNum=*on
037100071030     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037200071030     C                   Z-ADD(H)  PiVal         VABVLB
037300071030     C                   ELSE
037400071030     C                   SETON                                        32
037500071030     C                   Z-ADD     *zeros        VABVLB
037600071030     C                   EVAL      vinmsg = %trimr(vinmsg)
037700071030     C                             + ' ' + 'VABVLB'
037800071030     C                   ENDIF
037900071029     C* PKB
038000071030     C                   EVAL      PiStr=%trim(%subst(vindta:107:8))
038100010607     C                   EXSR      CHKNUM
038200010607     C                   IF        PiNum=*on
038300071030     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
038400071003     C                   Z-ADD(H)  PiVal         VABPKB
038500010607     C                   ELSE
038600010607     C                   SETON                                        32
038700010607     C                   Z-ADD     *zeros        VABPKB
038800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038900010607     C                             + ' ' + 'VABPKB'
039000010607     C                   ENDIF
039100071022     C* CAS
039200071030     C                   IF        %trim(%subst(vindta:183:10)) <> *blanks AND
039300071030     C                             %trim(%subst(vindta:183:10)) <> *zeros  AND
039400071030     C                             %trim(%subst(vindta:183:10)) <> '0'
039500071022     C                   EVAL      FlgCAS = '1'
039600071030     C                   EVAL      PiStr=%trim(%subst(vindta:183:10))
039700071022     C                   EXSR      CHKNUM
039800071022     C                   IF        PiNum=*on
039900071030     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
040000071022     C                   Z-ADD(H)  PiVal         VABCAS
040100071022     C                   EVAL      VABVCA ='EUR'
040200081007     C                   EVAL      VABTIC ='BM'
040300071022     C                   ELSE
040400071022     C                   SETON                                        32
040500071022     C                   Z-ADD     *zeros        VABCAS
040600071022     C                   EVAL      vinmsg = %trimr(vinmsg)
040700071022     C                             + ' ' + 'VABCAS'
040800071022     C                   ENDIF
040900071022     C                   ENDIF
041000020322     C*
041100020322     C* Considerazioni finali su CBO/CAS
041200050908     C                   IF        FlgCAS = '1'
041300040713     C                   IF        VABCBO = '1'
041400020322     C                   EVAL      VABCBO = '4'
041500040713     C                   ENDIF
041600040713     C                   IF        VABCBO = '2'
041700040713     C                   EVAL      VABCBO = '6'
041800040713     C                   ENDIF
041900020322     C                   ENDIF
042000020204     C*
042100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042200020204     C                   EXSR      CHKIMPDIV
042300020204     C*
042400010607     C                   ENDSR
042500010607     C*----------------------------------------------------*
042600010601
042700020204
042800020204     C*----------------------------------------------------*
042900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043000020204     C*----------------------------------------------------*
043100020204     C     CHKIMPDIV     BEGSR
043200020204     C*
043300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043400020204     C                   Z-ADD     *zeros        wrkDec            9 9
043500020204     C*
043600020204     C* Come prima cosa effettuo considerazioni sulla divisa
043700020204     C                   IF        vabIAS > *zeros
043800020204     C                   IF        vabVAS <> 'EUR'
043900020204     C                   EVAL      vabVAS =  'ITL'
044000020204     C                   ENDIF
044100020204     C                   ENDIF
044200020204     C*
044300020204     C                   IF        vabCAS > *zeros
044400020204     C                   IF        vabVCA <> 'EUR'
044500020204     C                   EVAL      vabVCA =  'ITL'
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C                   IF        vabVMD > *zeros
045000020321     C                   IF        vabVAD <> 'EUR'
045100020204     C                   EVAL      vabVAD =  'ITL'
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045600020204     C                   Z-ADD     vabIAS        wrkDec
045700020204     C                   IF        wrkDec > *zeros
045800020204     C                   IF        vabVAS = 'ITL'
045900020204     C                   EVAL      vabIAS = *zeros
046000020204     C                   ENDIF
046100020204     C                   ENDIF
046200020204     C*
046300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046400020204     C                   Z-ADD     vabCAS        wrkDec
046500020204     C                   IF        wrkDec > *zeros
046600020204     C                   IF        vabVCA = 'ITL'
046700020204     C                   EVAL      vabCAS = *zeros
046800020204     C                   ENDIF
046900020204     C                   ENDIF
047000020204     C*
047100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047200020204     C                   Z-ADD     vabVMD        wrkDec
047300020204     C                   IF        wrkDec > *zeros
047400020204     C                   IF        vabVAD = 'ITL'
047500020204     C                   EVAL      vabVMD = *zeros
047600020204     C                   ENDIF
047700020204     C                   ENDIF
047800020204     C*
047900020204     C                   ENDSR
048000020204     C***
048100020204
048200010330
048300010330
048400010330     C*----------------------------------------------------*
048500010330     C*  CONTROLLO NUMERICITA' CAMPI
048600010330     C*----------------------------------------------------*
048700010330     C     CHKNUM        BEGSR
048800010330     C*
048900010606     C                   IF        PiDecChr = *blanks
049000070502     C                   EVAL      PiDecChr = '.'
049100010606     C                   ENDIF
049200010606     C*
049300010606     C                   CALL(e)   'ISNUMERIC'
049400010330     C                   PARM                    PiStr            30
049500010606     C                   PARM                    PiDecChr          1
049600010330     C                   PARM      *ZEROS        PiVal            30 9
049700010330     C                   PARM      '0'           PiInt             1
049800010330     C                   PARM      '0'           PiNum             1
049900010330     C                   IF        %error
050000010606     C                   EVAL      PiNum=*off
050100010330     C                   ENDIF
050200010330     C*
050300010330     C                   ENDSR
050400010330     C***
050500140129
050600140129
050700140129     C*----------------------------------------------------*
050800140129     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050900140129     C*----------------------------------------------------*
051000140129     C     PREVAT        BEGSR
051100140129     C*
051200140129     C* Compongo il nome del membro da dare al FIVATWWR
051300140129     C                   eval      parmbr = vlrhdl
051400140129     C                   movel     'M'           parmbr
051500140129     C                   eval      parccm = %subst(vlrKSC:2:7)
051600140129     C                   eval      paropz = '1'
051700140129     C* Effettuo la chiamata al CLLE preposto
051800140129     C                   call(e)   'TITVVTC'
051900140129     C                   parm                    parccm
052000140129     C                   parm                    parmbr
052100140129     C                   parm                    paropz
052200140129     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052300140129     C                   if        %error
052400140129     C                   movel     '1'           chkcall
052500140129     C                   else
052600140129     C                   movel     '0'           chkcall
052700140129     C                   endif
052800140129     C*
052900140129     C                   ENDSR
053000140129
053100140129
053200140129
053300140129     C*----------------------------------------------------*
053400140129     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
053500140129     C*----------------------------------------------------*
053600140129     C     WRIVAT_E      BEGSR
053700140129     C*
053800140129     C* Valorizzo il buffer del file dettagli barcode colli
053900140129     C                   EVAL      VATFGS = VABFGS
054000140129     C                   EVAL      VATAAS = VABAAS
054100140129     C                   EVAL      VATLNP = VABLNP
054200140129     C                   EVAL      VATNSP = VABNSP
054300140129     C                   EVAL      VATCCM = VABCCM
054400140129     C                   EVAL      w40=%trim(%subst(vindta:210:40))
054500140129     C*
054600140129     C                   EVAL      VATTRC='I'
054700140129     C                   eval      VATNOT =
054800140129     C                                    %subst(w40:1:35)
054900140129     C                   exsr      wrivat                                       => scarico VAT
055000140129     C*
055100140129     C                   EVAL      VATTRC='J'
055200140129     C                   eval      VATNOT =
055300140129     C                                    %subst(w40:36:5)
055400140129     C                   exsr      wrivat                                       => scarico VAT
055500140129     C*
055600140129     C                   ENDSR
055700140129     C*----------------------------------------------------*
055800140129     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
055900140129     C*----------------------------------------------------*
056000140129     C     WRIVAT        BEGSR
056100140129     C*
056200140129     C* Scrivo solo se valorizzato qualcosa
056300140129     C                   IF        VATNOT <> *blanks
056400140129     C                   WRITE     FIVAT000
056500140129     C                   ENDIF
056600140129     C*
056700140129     C                   ENDSR
056800010330
056900010601
057000010601
057100010601
057200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057300010601     C     repfil        BEGSR
057400010601     C*
057500010601     C                   if        invfil = *zeros and
057600010601     C                             depfil > *zeros and
057700010629     C                             (vinflg = *blanks or
057800010629     C                              vinflg = *zeros)
057900010601     C
058000010601     C                   eval      invfil = depfil
058100010601     C                   endif
058200010601     C*
058300010601     C                   if        depfil <> invfil and
058400010601     C                             invfil > *zeros
058500010601     C                   eval      flgMulti = '1'
058600010601     C                   if        vinflg = *blanks
058700010601     C                   add       1             cntNonEl
058800010601     C                   endif
058900010601     C                   endif
059000010601     C*
059100010601     C                   if        vinflg = '2'
059200010601     C                   eval      flgStato = '2'
059300010601     C                   endif
059400010601     C*
059500010601     C                   ENDSR
059600010601     C***
059700010601
059800010601
059900010601
060000010330
060100010330
060200010330
060300990920      /TITLE Invio dei dati al punto operativo.
060400000613     C     invio         BEGSR
060500140129     C*
060600140129     C* 1° invio FIVAT
060700140129     C                   reset                   dscmz
060800140129     C                   move      vlrpoi        cmzdst
060900140129     C                   eval      cmzfld = 'FIVATWWR'
061000140129     C                   eval      cmzmbd = vlrhdl
061100140129     C                   eval      %subst(cmzmbd:1:1) = 'M'
061200140129     C***                if        prmfir = *blanks
061300140129     C                   eval      cmzfla = 'FIVAT00F'
061400140129     C                   eval      cmzmba = 'FIVAT00F'
061500140129     C***                else
061600140129     C***                eval      cmzfla = prmfir
061700140129     C***                eval      cmzmba = prmfir
061800140129     C***                endif
061900140129     C                   eval      cmznrr = *zeros
062000140129     C                   move      §ctrok        cmznrr
062100140129     C                   eval      cmzlba = vlrfl1
062200140129     C                   call(e)   'TIS711C'
062300140129     C                   parm                    dscmz
062400140129     C                   parm      *blanks       esito
062500140129     C                   if        %error
062600140129     C                             or cmzerr = '1'
062700140129     C                             or esito  = '1'
062800140129     C                   eval      wrkesito = '3'
062900140129     C                   else
063000140129     C*
063100140129     C* 2° invio FIVAB
063200990920     C                   reset                   dscmz
063300010601     C                   move      invfil        cmzdst
063400990920     C                   eval      cmzfld = vlrfou
063500990920     C                   eval      cmzmbd = vlrhdl
063600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063700140129     C***                if        prmfir = *blanks
063800050408     C                   eval      cmzfla = 'FIVAB00F'
063900050408     C                   eval      cmzmba = 'FIVAB00F'
064000140129     C***                else
064100140129     C***                eval      cmzfla = prmfir
064200140129     C***                eval      cmzmba = prmfir
064300140129     C***                endif
064400990920     C                   eval      cmznrr = *zeros
064500990920     C                   move      §ctrok        cmznrr
064600021018     C                   eval      cmzlba = vlrfl1
064700990920     C                   call(e)   'TIS711C'
064800990920     C                   parm                    dscmz
064900990921     C                   parm      *blanks       esito
065000990920     C                   if        %error
065100990920     C                             or cmzerr = '1'
065200990921     C                             or esito  = '1'
065300000710     C                   eval      wrkesito = '3'
065400990920     C                   endif
065500140129     C                   endif
065600990920     C*
065700000613     C                   ENDSR
065800990910
065900010601
066000010601
066100010601
066200010601
066300010601      /TITLE Invio dei dati al punto operativo.
066400010601     C     opeini        BEGSR
066500010601     C*
066600010601     C* Inizializzo flag e contatori operativi
066700010601     C                   movel     '0'           flgGiro           1
066800010601     C                   movel     '0'           flgMulti          1
066900010601     C                   movel     '1'           flgStato          1
067000010615     C                   movel     '0'           flgOk             1
067100010601     C                   z-add     *zeros        cntNonEl         10 0
067200010601     C                   z-add     *zeros        depfil            3 0
067300010601     C                   z-add     *zeros        invfil            3 0
067400010601     C*
067500010601     C                   ENDSR
067600010601     C***
067700010601
067800010601
067900140129     C     *pssr         BEGSR
068000140129     C*
068100140129     C                   if        %open(tivin00r)
068200140129     C                   close     tivin00r
068300140129     C                   endif
068400140129     C                   if        %open(fivabwwr)
068500140129     C                   close     fivabwwr
068600140129     C                   endif
068700140129     C                   if        %open(fivatwwr)
068800140129     C                   close     fivatwwr
068900140129     C                   endif
069000140129     C*
069100140129     C* Effettuo la chiamata al CLLE preposto
069200140129     C                   call(e)   'TITVVTC'
069300140129     C                   parm                    parccm
069400140129     C                   parm                    parmbr
069500140129     C                   parm      '2'           paropz
069600140129     C*
069700140129     C                   eval      wrkesito = '2'
069800140129     C*
069900140129     C                   seton                                        LR
070000140129     C*
070100140129     C                   ENDSR     '*CANCL'
070200140129     C***
070300010601
070400010330
070500010330
070600000613     C     *inzsr        BEGSR
070700990910     C*
070800990910     C     *entry        plist
070900990920     C                   parm                    tivlrds
071000990921     C                   parm      wrkesito      esito
071100000724     C                   parm                    prmlit
071200000710     C                   parm                    prmfir
071300010330     C*
071400010330     C* CALCOLA LA DATA CORRENTE
071500010330     C                   time                    wn14             14 0
071600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071700010330     C                   z-add     wn8           g08dat
071800010330     C                   z-add     *zeros        g08inv
071900010330     C                   movel     '0'           g08err
072000010330     C                   call      'XSRDA8'
072100010330     C                   parm                    wlbda8
072200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072300000613     C*
072400000613     C                   ENDSR
072500000613     C***
