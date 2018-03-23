000100050408      /TITLE Upload via Internet: traduzione in EDIVABWR. (x FABER  Cod. 0660701)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FEDIVABwr  O    E             DISK    usropn
000600050408     FEDIVATwr  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800010330     D wrkesito        s                   like(esito)
001900010330     D rrnum           s              6  0 INZ(*zeros)
002000050217     D parccm          s              8    INZ(*blanks)
002100050217     D parmbr          s             10    INZ(*blanks)
002200050217     D paropz          s              1    INZ(*blanks)
002300050217     D chkcall         s              1    INZ(*blanks)
002400050217     D wNOTE           s            100    INZ(*blanks)
002500010330     D*------------------
002600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700010330     D*------------------
002800010330     D WLBDA8          DS                  INZ
002900010330     D  G08DAT                 1      8  0
003000010330     D  G08INV                 9     16  0
003100010330     D  G08ERR                17     17
003200010330     D  G08TGI                18     22  0
003300000313     D*
003400010330
003500010330
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010601     C*
004000010601     C                   exsr      opeini
004100010605     C                   exsr      rwvab
004200050217     C*
004300050217     C* Effettuo la chiamata al CLLE preposto
004400050412     C                   call(e)   'TITVEVTC'
004500050217     C                   parm                    parccm
004600050217     C                   parm                    parmbr
004700050217     C                   parm      '2'           paropz
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
008400050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVAB0F              *
008500010330     C*--------------------------------------------------------
008600010605     C     RWVAB         BEGSR
008700010330     C*
008800010330     C                   if        not %open(tivin00r)
008900010330     C                   open      tivin00r
009000010330     C                   endif
009100050408     C                   if        not %open(edivabwr)
009200050408     C                   open      edivabwr
009300010330     C                   endif
009400050217     C*
009500050408     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009600050217     C                   exsr      prevat
009700050217     C*
009800050217     C                   if        chkcall = '0'
009900050217     C*
010000050408     C                   if        not %open(edivatwr)
010100050408     C                   open      edivatwr
010200050217     C                   endif
010300010330     C*
010400010604     C                   clear                   §CTROK
010500010604     C                   clear                   §CTRMO
010600010604     C                   clear                   §CTRNO
010700010330     C*
010800010330     C                   DO        *HIVAL
010900010330     C*
011000010330     C                   READ      tivin00r                               70
011100010618     C*
011200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011300010618     C                   if        vinflg = '1'
011400010618     C                   eval      flgOk = '1'
011500010618     C                   endif
011600010618     C*
011700010330     C                   if        vindta > *blanks
011800010330     C                   add       1             rrnum
011900010330     C*
012000010601     C                   if        *in70 = *off and
012100010330     C                             (vinflg = *blanks
012200010330     C                              or vinflg = '0'
012300010330     C                              or vinflg = '2')
012400050217     C*
012500050408     C                   clear                   EDIVAB00
012600050408     C                   clear                   EDIVAT00
012700010330     C*
012800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012900010711     C                   if        vinflg = *blanks or vinflg = '0'
013000010711     C                   clear                   vinmsg
013100010711     C                   endif
013200010601     C*
013300010330     C                   exsr      inzvar
013400010330     C                   exsr      defcam
013500010605     C                   exsr      impvab
013600010601     C*
013700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013800010605     C                   eval      depfil = VABLNP
013900010601     C                   exsr      repfil
014000010601     C                   if        depfil = invfil
014100021025     C                   if        vlrpoi = 999
014200021025     C                   MOVE(P)   invfil        VABFGS
014300021025     C                   else
014400021025     C                   MOVE(P)   vlrpoi        VABFGS
014500021025     C                   endif
014600050114     C                   endif
014700010601     C*
014800010601     C                   exsr      PREELA
014900050408     C*
015000050408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
015100050408     C                   EVAL      VABCMR = 'FABER'
015200050408     C                   EVAL      VABDCM = DATCOR
015300050408     C                   EVAL      VABDTS = DATCOR
015400050408     C                   EVAL      VABHMS = ORACOR
015500050408     C                   EVAL      VABCNT = 1
015600050408     C*
015700050408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
015800050408     C                   EVAL      VATCMR = 'FABER'
015900050408     C                   EVAL      VATCNT = 1
016000010601     C*
016100010604     C* Ebbene...
016200010604     C*
016300010604     C  N31              ADD       1             §CTROK            7 0
016400010604     C   32              ADD       1             §CTRMO            7 0
016500010604     C   31              ADD       1             §CTRNO            7 0
016600020205     C*
016700050408     C* Scarico il buffer del EDIVAB
016800050408     C  N31              WRITE     EDIVAB00
016900050217     C  N31              EXSR      WRIVAT
017000010604     C*
017100020722     C                   endif
017200020722     C*
017300010604     C                   if        *in31 = *off and
017400010604     C                             *in32 = *off
017500010604     C                   eval      vinflg = '1'
017600010604     C                   else
017700010604     C                   eval      vinflg = '2'
017800010604     C                   endif
017900010604     C*
018000010330     C                   else
018100010330     C                   eval      vinflg = '1'
018200010330     C                   endif
018300010601     C*
018400010601     C  N70              update    tivin000
018500010330     C*
018600010330     C  N70              ENDdo
018700010601     C
018800010601     C*
018900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019000010601     C                   if        cntNonEl = *zeros or
019100010601     C                             flgMulti = '0'
019200010330     C* Se non ci sono record con errori ...
019300010601     C                   if        §ctrno = 0 and
019400010604     C                             §ctrmo = 0 and
019500010601     C                             flgStato <> '2'
019600010330     C* ... restituisco esito OK.
019700010330     C                   eval      wrkesito = '0'
019800010330     C                   else
019900010330     C                   if        §ctrok > 0
020000010330     C                   eval      wrkesito = '1'
020100010330     C                   else
020200010615     C                   if        flgOk = '0'
020300010615     C                   eval      wrkesito = '2'
020400010615     C                   else
020500010615     C                   eval      wrkesito = '6'
020600010615     C                   endif
020700010330     C                   endif
020800010330     C                   endif
020900010601     C                   else
021000010601     C                   eval      wrkesito = '9'
021100010601     C                   endif
021200050217     C*
021300050217     C                   else
021400050217     C                   eval      wrkesito = '2'
021500050217     C                   endif
021600010330     C*
021700010330     C                   if        %open(tivin00r)
021800010330     C                   close     tivin00r
021900010330     C                   endif
022000050408     C                   if        %open(edivabwr)
022100050408     C                   close     edivabwr
022200010330     C                   endif
022300050408     C                   if        %open(edivatwr)
022400050408     C                   close     edivatwr
022500050217     C                   endif
022600010601     C*
022700010601     C                   if        vlrpoi <> 999
022800010601     C                   eval      invfil = vlrpoi
022900010601     C                   endif
023000010330     C*
023100010330     C                   if        §ctrok > 0
023200010601     C                             and invfil > *zeros
023300010330     C                   exsr      invio
023400010330     C                   endif
023500010601     C*
023600010618     C                   if        flgGiro = '1'
023700010601     C                   exsr      endela
023800010618     C                   endif
023900010330     C*
024000010330     C                   ENDSR
024100010330     C***
024200010330
024300010601
024400010601
024500010601
024600010601
024700010330     C*----------------------------------------------------*
024800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024900010330     C*----------------------------------------------------*
025000010330     C     INZVAR        BEGSR
025100010330     C*
025200020204     C                   Z-ADD     *zeros        Num5_0            5 0
025300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
025400020322     C                   MOVEL     '0'           FlgCAS            1
025500010330     C*
025600010330     C                   ENDSR
025700010330     C*----------------------------------------------------*
025800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
025900010330     C*----------------------------------------------------*
026000010330     C     DEFCAM        BEGSR
026100010330     C*
026200020204     C* Inizializzo il buffer del record da scrivere
026300050408     C                   CLEAR                   EDIVAT00
026400020204     C* Imposto i valori di default...
026500050217     C                   EVAL      VABCCM = 0660701
026600050217     C                   EVAL      VABLNP = 066
026700040713     C                   EVAL      VABCTR = 000
026800041108     C                   EVAL      VABCBO = '1'
026900050217     C                   EVAL      VABCTM = '7Q'
027000020204     C* ... e poi verifico se sono stati passati come parametri
027100020204     C                   IF        vlrppt > *blanks
027200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
027300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027400020204     C                   EXSR      CHKNUM
027500020204     C                   IF        PiInt=*on
027600020204     C                   Z-ADD     PiVal         VABCCM
027700020204     C                   ENDIF
027800040301     C                   ENDIF
027900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
028000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028100020204     C                   EXSR      CHKNUM
028200020204     C                   IF        PiInt=*on
028300020204     C                   Z-ADD     PiVal         VABLNP
028400020204     C                   ENDIF
028500040301     C                   ENDIF
028600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
028700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028800020204     C                   EXSR      CHKNUM
028900020204     C                   IF        PiInt=*on
029000020204     C                   Z-ADD     PiVal         VABCTR
029100040301     C                   ENDIF
029200020204     C                   ENDIF
029300020204     C                   ENDIF
029400020204     C*
029500010330     C                   ENDSR
029600010607     C*----------------------------------------------------*
029700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
029800010607     C*----------------------------------------------------*
029900010607     C     IMPVAB        BEGSR
030000010607     C*
030100010607     C                   SETOFF                                       3132
030200010607     C*
030300010607     C* Reperimento campi ALFA
030400010607     C*
030500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
030600050217     C                   EVAL      VABRSD=%trim(%subst(vindta:103:30))
030700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030900020117     C* ==
031000050217     C                   EVAL      VABIND=%trim(%subst(vindta:185:60))+' '+
031100050217     C                                    %trim(%subst(vindta:245:10))
031200050217     C                   EVAL      VABLOD=%trim(%subst(vindta:133:40))
031300050217     C                   EVAL      VABPRD=%trim(%subst(vindta:258:3))
031400080825     C                   EVAL      VABRMA=%trim(%subst(vindta:36:10))
031500050217     C                   EVAL      wNOTE =%trim(%subst(vindta:400:100))
031600050217     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
031700050217     C                   EVAL      VABNT2=%subst(wNOTE:1+35:35)
031800010607     C*
031900010607     C* Reperimento campi NUMERICI
032000020722     C                   MOVEL     DATCOR        VABAAS
032100020722     C                   MOVE      DATCOR        VABMGS
032200040420     C* NSP/RMN
032300080825     C                   EVAL      PiStr=%trim(%subst(vindta:36:10))
032400020304     C                   EXSR      CHKNUM
032500020304     C                   IF        PiInt=*on
032600040301     C                   Z-ADD     PiVal         VABNSP
032700040301     C                   Z-ADD     PiVal         VABRMN
032800020304     C                   ELSE
032900050217     C                   SETON                                        31
033000050217     C                   Z-ADD     *zeros        VABNSP
033100050217     C                   Z-ADD     *zeros        VABRMN
033200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
033300040301     C                             + ' ' + 'VABNSP VABRMN'
033400020304     C                   ENDIF
033500050128     C* CAD
033600050217     C                   EVAL      PiStr=%trim(%subst(vindta:173:12))
033700050128     C                   EXSR      CHKNUM
033800050128     C                   IF        PiInt=*on
033900050128     C                   Z-ADD     PiVal         Num5_0
034000050128     C                   MOVEL(p)  Num5_0        VABCAD
034100050128     C                   ELSE
034200050128     C                   SETON                                        32
034300050128     C                   EVAL      VABCAD = *zeros
034400050128     C                   EVAL      vinmsg = %trimr(vinmsg)
034500050128     C                             + ' ' + 'VABCAD'
034600050128     C                   ENDIF
034700040420     C* NCL
034800050217     C                   EVAL      PiStr=%trim(%subst(vindta:88:5))
034900010607     C                   EXSR      CHKNUM
035000010607     C                   IF        PiInt=*on
035100010607     C                   Z-ADD     PiVal         VABNCL
035200010607     C                   ELSE
035300010607     C                   SETON                                        32
035400010607     C                   Z-ADD     *zeros        VABNCL
035500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035600010607     C                             + ' ' + 'VABNCL'
035700010607     C                   ENDIF
035800090210     C* CTR
035900090210     C                   IF        %subst(vindta:500:3) = '001'
036000090210     C                   EVAL      PiStr=%trim(%subst(vindta:500:3))
036100090210     C                   EXSR      CHKNUM
036200090210     C                   IF        PiInt=*on
036300090210     C                   Z-ADD     PiVal         VABCTR
036400090210     C                   ELSE
036500090210     C                   SETON                                        32
036600090210     C                   Z-ADD     *zeros        VABCTR
036700090210     C                   EVAL      vinmsg = %trimr(vinmsg)
036800090210     C                             + ' ' + 'VABCTR'
036900090210     C                   ENDIF
037000090210     C                   ENDIF
037100040420     C* PKB
037200050217     C                   EVAL      PiStr=%trim(%subst(vindta:321:15))
037300010607     C                   EXSR      CHKNUM
037400010607     C                   IF        PiNum=*on
037500050114     C                   Z-ADD     PiVal         VABPKB
037600010607     C                   ELSE
037700010607     C                   SETON                                        32
037800010607     C                   Z-ADD     *zeros        VABPKB
037900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038000010607     C                             + ' ' + 'VABPKB'
038100010607     C                   ENDIF
038200091124     C* VLB
038300091124     C                   EVAL      PiStr=%trim(%subst(vindta:354:15))
038400091124     C                   EXSR      CHKNUM
038500091124     C                   IF        PiNum=*on
038600091124     C                   Z-ADD     PiVal         VABVLB
038700091124     C                   ELSE
038800091124     C                   SETON                                        32
038900091124     C                   Z-ADD     *zeros        VABVLB
039000091124     C                   EVAL      vinmsg = %trimr(vinmsg)
039100091124     C                             + ' ' + 'VABVLB'
039200091124     C                   ENDIF
039300050216     C* CAS
039400050217     C                   IF        %trim(%subst(vindta:372:15)) <> '0,00'
039500050216     C                   MOVEL     '1'           FlgCAS
039600050217     C                   EVAL      PiStr=%trim(%subst(vindta:372:15))
039700050217     C                   EVAL      VABVCA='EUR'
039800050216     C                   EXSR      CHKNUM
039900050216     C                   IF        PiNum=*on
040000050216     C                   Z-ADD     PiVal         VABCAS
040100050216     C                   ELSE
040200050216     C                   SETON                                        32
040300050216     C                   EVAL      vinmsg = %trimr(vinmsg)
040400050216     C                             + ' ' + 'VABCAS'
040500050216     C                   ENDIF
040600050216     C                   ENDIF
040700020322     C*
040800020322     C* Considerazioni finali su CBO/CAS
040900020322     C                   IF        FlgCAS = '1'
041000040713     C                   IF        VABCBO = '1'
041100020322     C                   EVAL      VABCBO = '4'
041200040713     C                   ENDIF
041300040713     C                   IF        VABCBO = '2'
041400040713     C                   EVAL      VABCBO = '6'
041500040713     C                   ENDIF
041600020322     C                   ENDIF
041700020204     C*
041800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041900020204     C                   EXSR      CHKIMPDIV
042000020204     C*
042100010607     C                   ENDSR
042200010607     C*----------------------------------------------------*
042300010601
042400020204
042500020204     C*----------------------------------------------------*
042600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042700020204     C*----------------------------------------------------*
042800020204     C     CHKIMPDIV     BEGSR
042900020204     C*
043000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043100020204     C                   Z-ADD     *zeros        wrkDec            9 9
043200020204     C*
043300020204     C* Come prima cosa effettuo considerazioni sulla divisa
043400020204     C                   IF        vabIAS > *zeros
043500020204     C                   IF        vabVAS <> 'EUR'
043600020204     C                   EVAL      vabVAS =  'ITL'
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C                   IF        vabCAS > *zeros
044100020204     C                   IF        vabVCA <> 'EUR'
044200020204     C                   EVAL      vabVCA =  'ITL'
044300020204     C                   ENDIF
044400020204     C                   ENDIF
044500020204     C*
044600020204     C                   IF        vabVMD > *zeros
044700020321     C                   IF        vabVAD <> 'EUR'
044800020204     C                   EVAL      vabVAD =  'ITL'
044900020204     C                   ENDIF
045000020204     C                   ENDIF
045100020204     C*
045200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045300020204     C                   Z-ADD     vabIAS        wrkDec
045400020204     C                   IF        wrkDec > *zeros
045500020204     C                   IF        vabVAS = 'ITL'
045600020204     C                   EVAL      vabIAS = *zeros
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046100020204     C                   Z-ADD     vabCAS        wrkDec
046200020204     C                   IF        wrkDec > *zeros
046300020204     C                   IF        vabVCA = 'ITL'
046400020204     C                   EVAL      vabCAS = *zeros
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046900020204     C                   Z-ADD     vabVMD        wrkDec
047000020204     C                   IF        wrkDec > *zeros
047100020204     C                   IF        vabVAD = 'ITL'
047200020204     C                   EVAL      vabVMD = *zeros
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C                   ENDSR
047700020204     C***
047800020204
047900010330
048000010330
048100010330     C*----------------------------------------------------*
048200010330     C*  CONTROLLO NUMERICITA' CAMPI
048300010330     C*----------------------------------------------------*
048400010330     C     CHKNUM        BEGSR
048500010330     C*
048600010606     C                   IF        PiDecChr = *blanks
048700050217     C                   EVAL      PiDecChr = ','
048800010606     C                   ENDIF
048900010606     C*
049000010606     C                   CALL(e)   'ISNUMERIC'
049100010330     C                   PARM                    PiStr            30
049200010606     C                   PARM                    PiDecChr          1
049300010330     C                   PARM      *ZEROS        PiVal            30 9
049400010330     C                   PARM      '0'           PiInt             1
049500010330     C                   PARM      '0'           PiNum             1
049600010330     C                   IF        %error
049700010606     C                   EVAL      PiNum=*off
049800010330     C                   ENDIF
049900010330     C*
050000010330     C                   ENDSR
050100010330     C***
050200050217
050300050217
050400050217
050500050217     C*----------------------------------------------------*
050600050408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
050700050217     C*----------------------------------------------------*
050800050217     C     PREVAT        BEGSR
050900050217     C*
051000050408     C* Compongo il nome del membro da dare al EDIVATWR
051100050217     C                   eval      parmbr = vlrhdl
051200050217     C                   movel     'M'           parmbr
051300050217     C                   eval      parccm = %subst(vlrKSC:2:7)
051400050217     C                   eval      paropz = '1'
051500050217     C* Effettuo la chiamata al CLLE preposto
051600050412     C                   call(e)   'TITVEVTC'
051700050217     C                   parm                    parccm
051800050217     C                   parm                    parmbr
051900050217     C                   parm                    paropz
052000050217     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052100050217     C                   if        %error
052200050217     C                   movel     '1'           chkcall
052300050217     C                   else
052400050217     C                   movel     '0'           chkcall
052500050217     C                   endif
052600050217     C*
052700050217     C                   ENDSR
052800050217
052900050217
053000050217
053100050217     C*----------------------------------------------------*
053200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
053300050217     C*----------------------------------------------------*
053400050217     C     WRIVAT        BEGSR
053500050217     C*
053600050217     C* Valorizzo il buffer del file dettagli barcode colli
053700050217     C                   EVAL      VATFGS = VABFGS
053800050217     C                   EVAL      VATAAS = VABAAS
053900050217     C                   EVAL      VATLNP = VABLNP
054000050217     C                   EVAL      VATNSP = VABNSP
054100050217     C                   EVAL      VATCCM = VABCCM
054200050217     C                   EVAL      VATTRC='B'
054300050217     C                   EVAL      VATNOT = %trim(%subst(vindta:261:30))
054400050217     C                   IF        VATNOT <> *blanks
054500050408     C                   write     EDIVAT00
054600050217     C                   ENDIF
054700050217     C*
054800050217     C                   ENDSR
054900010330
055000010601
055100010601
055200010601
055300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
055400010601     C     repfil        BEGSR
055500010601     C*
055600010601     C                   if        invfil = *zeros and
055700010601     C                             depfil > *zeros and
055800010629     C                             (vinflg = *blanks or
055900010629     C                              vinflg = *zeros)
056000010601     C
056100010601     C                   eval      invfil = depfil
056200010601     C                   endif
056300010601     C*
056400010601     C                   if        depfil <> invfil and
056500010601     C                             invfil > *zeros
056600010601     C                   eval      flgMulti = '1'
056700010601     C                   if        vinflg = *blanks
056800010601     C                   add       1             cntNonEl
056900010601     C                   endif
057000010601     C                   endif
057100010601     C*
057200010601     C                   if        vinflg = '2'
057300010601     C                   eval      flgStato = '2'
057400010601     C                   endif
057500010601     C*
057600010601     C                   ENDSR
057700010601     C***
057800050217
057900050217
058000050217
058100050217
058200050217      /TITLE Invio dei dati al punto operativo.
058300050217     C     invio         BEGSR
058400050217     C*
058500050408     C* 1° invio EDIVAT
058600050217     C                   reset                   dscmz
058700050217     C                   move      vlrpoi        cmzdst
058800050408     C                   eval      cmzfld = 'EDIVATWR'
058900050412     C                   eval      cmzmbd = parmbr
059000050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
059100050217     C***                if        prmfir = *blanks
059200050408     C                   eval      cmzfla = 'EDIVAT0F'
059300050408     C                   eval      cmzmba = 'EDIVAT0F'
059400050217     C***                else
059500050217     C***                eval      cmzfla = prmfir
059600050217     C***                eval      cmzmba = prmfir
059700050217     C***                endif
059800050217     C                   eval      cmznrr = *zeros
059900050217     C                   move      §ctrok        cmznrr
060000050217     C                   eval      cmzlba = vlrfl1
060100050217     C                   call(e)   'TIS711C'
060200050217     C                   parm                    dscmz
060300050217     C                   parm      *blanks       esito
060400050217     C                   if        %error
060500050217     C                             or cmzerr = '1'
060600050217     C                             or esito  = '1'
060700050217     C                   eval      wrkesito = '3'
060800050217     C                   else
060900050217     C*
061000050408     C* 2° invio EDIVAB
061100050217     C                   reset                   dscmz
061200050217     C                   move      vlrpoi        cmzdst
061300050217     C                   eval      cmzfld = vlrfou
061400050217     C                   eval      cmzmbd = vlrhdl
061500050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
061600050217     C***                if        prmfir = *blanks
061700050408     C                   eval      cmzfla = 'EDIVAB0F'
061800050408     C                   eval      cmzmba = 'EDIVAB0F'
061900050217     C***                else
062000050217     C***                eval      cmzfla = prmfir
062100050217     C***                eval      cmzmba = prmfir
062200050217     C***                endif
062300050217     C                   eval      cmznrr = *zeros
062400050217     C                   move      §ctrok        cmznrr
062500050217     C                   eval      cmzlba = vlrfl1
062600050217     C                   call(e)   'TIS711C'
062700050217     C                   parm                    dscmz
062800050217     C                   parm      *blanks       esito
062900050217     C                   if        %error
063000050217     C                             or cmzerr = '1'
063100050217     C                             or esito  = '1'
063200050217     C                   eval      wrkesito = '3'
063300050217     C                   endif
063400050217     C                   endif
063500050217     C*
063600050217     C                   ENDSR
063700050217     C***
063800050217
063900050217
064000050217
064100010601
064200010601
064300010601      /TITLE Invio dei dati al punto operativo.
064400010601     C     opeini        BEGSR
064500010601     C*
064600010601     C* Inizializzo flag e contatori operativi
064700010601     C                   movel     '0'           flgGiro           1
064800010601     C                   movel     '0'           flgMulti          1
064900010601     C                   movel     '1'           flgStato          1
065000010615     C                   movel     '0'           flgOk             1
065100010601     C                   z-add     *zeros        cntNonEl         10 0
065200010601     C                   z-add     *zeros        depfil            3 0
065300010601     C                   z-add     *zeros        invfil            3 0
065400010601     C*
065500010601     C                   ENDSR
065600010601     C***
065700010601
065800010601
065900010601
066000010330
066100010330
066200000613     C     *inzsr        BEGSR
066300990910     C*
066400990910     C     *entry        plist
066500990920     C                   parm                    tivlrds
066600990921     C                   parm      wrkesito      esito
066700000724     C                   parm                    prmlit
066800000710     C                   parm                    prmfir
066900010330     C*
067000010330     C* CALCOLA LA DATA CORRENTE
067100010330     C                   time                    wn14             14 0
067200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067300050408     C                   movel     wn14          oracor            6 0          *ORA
067400010330     C                   z-add     wn8           g08dat
067500010330     C                   z-add     *zeros        g08inv
067600010330     C                   movel     '0'           g08err
067700010330     C                   call      'XSRDA8'
067800010330     C                   parm                    wlbda8
067900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068000000613     C*
068100000613     C                   ENDSR
068200000613     C***
