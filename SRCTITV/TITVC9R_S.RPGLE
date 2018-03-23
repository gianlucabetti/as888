000100040706      /TITLE Upload via Internet: traduzione in FIVABWWR. x GRUPPO IRCA (ZOPPAS)
000200040706     H*                           Cod. 0590376 / 0590499 / 0590422 / 0590502 / 0590098 / 0590132
000300060523     H*                                0592665 / 0592966 / 0592967 / ...
000400990908     H dftactgrp(*yes)
000500000313     F*
000600990910     Ftivin00r  uF   E             DISK    usropn
000700021014     FFIVABwwr  O    E             DISK    usropn
000800000313     D*
000900010330     D*----------------------------------------------------
001000010330     D* DICHIARAZIOINE VARIABILI DI WRK
001100010330     D*----------------------------------------------------
001200010330     D dscmz         e ds                  inz
001300010330     D psds           sds
001400010330     D  procname         *PROC
001500010330     D tivlrds       e ds                  extname(tivlr00f)
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100010330     D*------------------
002200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002300010330     D*------------------
002400010330     D WLBDA8          DS                  INZ
002500010330     D  G08DAT                 1      8  0
002600010330     D  G08INV                 9     16  0
002700010330     D  G08ERR                17     17
002800010330     D  G08TGI                18     22  0
002900000313     D*
003000010330
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
022500040706     C                   EVAL      VABLNP = 059
022600040706     C                   EVAL      VABCTR = 000
022700040706     C                   EVAL      VABTSP = 'C'
022800040706     C                   EVAL      VABCBO = '1'
022900040707     C                   MOVEL     'I'           wIOE              1
023000060523     C                   MOVEL     'RICA'        wDIV              4
023100060523     C                   MOVEL     'F'           wPROD             1
023200020204     C*
023300010330     C                   ENDSR
023400010607     C*----------------------------------------------------*
023500021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
023600010607     C*----------------------------------------------------*
023700010607     C     IMPVAB        BEGSR
023800010607     C*
023900010607     C                   SETOFF                                       3132
024000010607     C*
024100010607     C* Reperimento campi ALFA
024200010607     C*
024300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
024400040706     C                   EVAL      VABRSD=%trim(%subst(vindta:225:35))
024500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024700020117     C* ==
024800040706     C                   EVAL      VABIND=%trim(%subst(vindta:260:35))
024900040706     C                   EVAL      VABLOD=%trim(%subst(vindta:300:25))
025000040706     C                   EVAL      VABPRD=%trim(%subst(vindta:325:2))
025100040707     C***                EVAL      VABNZD=%trim(%subst(vindta:327:4))
025200040708     C                   EVAL      VABRMA=%trim(%subst(vindta:7:16))
025300040706     C                   EVAL      VABNOT=%trim(%subst(vindta:388:35))
025400040706     C                   EVAL      VABNT2=%trim(%subst(vindta:388+35:35))
025500040706     C                   IF        %trim(%subst(vindta:347:10)) = 'VE019'
025600040706     C                   EVAL      VABTSP='C'
025700040706     C                   ENDIF
025800040706     C                   IF        %trim(%subst(vindta:347:10)) = 'VE094'
025900040706     C                   EVAL      VABTSP='E'
026000040706     C                   ENDIF
026100040706     C                   IF        %trim(%subst(vindta:382:4)) = 'DDU'
026200040706     C                   EVAL      VABCBO='1'
026300040706     C                   ENDIF
026400040706     C                   IF        %trim(%subst(vindta:382:4)) = 'EXW'
026500040706     C                   EVAL      VABCBO='2'
026600040706     C                   ENDIF
026700040706     C                   IF        %trim(%subst(vindta:386:2)) = 'SI'
026800040706     C                   EVAL      FlgCAS='1'
026900040706     C                   ENDIF
027000040707     C*
027100040707     C* Considerazioni particolari su spedizione ITALIA o ESTERO
027200060523     C                   IF        %subst(vindta:23:3) = 'DDT'
027300060523     C                   MOVEL     'I'           wIOE
027400060523     C                   ELSE
027500040707     C                   MOVEL     'E'           wIOE
027600040707     C                   ENDIF
027700060523     C*
027800060523     C* Considerazioni particolari su prodotto FINITO o c/to LAVORO
027900060523     C                   IF        wIOE = 'I'
028000060523     C                   IF        %subst(vindta:28:1) = '0' OR
028100060523     C                             %subst(vindta:28:1) = '2' OR
028200060523     C                             %subst(vindta:28:1) = '3'
028300060523     C                   MOVEL     'F'           wPROD
028400060523     C                   ELSE
028500060523     C                   IF        %subst(vindta:28:1) = '6' OR
028600060523     C                             %subst(vindta:28:1) = '7' OR
028700060523     C                             %subst(vindta:28:1) = '8'
028800060523     C                   MOVEL     'L'           wPROD
028900060523     C                   ENDIF
029000060523     C                   ENDIF
029100060523     C                   ENDIF
029200060523     C*
029300060523     C* Considerazioni particolari sulla DIVISIONE
029400060523     C                   IF        %subst(vindta:1:6) = '000021'
029500060523     C*    */ Divisione RICA */
029600060531     C                   MOVEL(P)  'RICA'        wDIV
029700060523     C                   ELSE
029800060523     C                   IF        %subst(vindta:1:6) = '000022'
029900060523     C*    */ Divisione SEV */
030000060531     C                   MOVEL(P)  'SEV'         wDIV
030100060523     C                   ELSE
030200060523     C                   IF        %subst(vindta:1:6) = '000023'
030300060523     C*    */ Divisione IRCA */
030400060531     C                   MOVEL(P)  'IRCA'        wDIV
030500060523     C                   ENDIF
030600060523     C                   ENDIF
030700060523     C                   ENDIF
030800060523     C*
030900060523     C* Considerazioni finali sull'attribuzione del codice cliente mittente
031000060523     C                   IF        wDIV = 'IRCA'
031100060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
031200060523     C                   EVAL      VABCCM = 0590098
031300060523     C                   ENDIF
031400060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
031500060523     C                   EVAL      VABCCM = 0592965
031600060523     C                   ENDIF
031700060523     C                   IF        wIOE = 'E'
031800060523     C                   EVAL      VABCCM = 0590132
031900060523     C                   ENDIF
032000060523     C                   ENDIF
032100060523     C*
032200060523     C                   IF        wDIV = 'RICA'
032300060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
032400060523     C                   EVAL      VABCCM = 0590376
032500060523     C                   ENDIF
032600060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
032700060523     C                   EVAL      VABCCM = 0592966
032800060523     C                   ENDIF
032900060523     C                   IF        wIOE = 'E'
033000060523     C                   EVAL      VABCCM = 0590499
033100060523     C                   ENDIF
033200060523     C                   ENDIF
033300060523     C*
033400060523     C                   IF        wDIV = 'SEV'
033500060523     C                   IF        wIOE = 'I' AND wPROD = 'F'
033600060523     C                   EVAL      VABCCM = 0590422
033700060523     C                   ENDIF
033800060523     C                   IF        wIOE = 'I' AND wPROD = 'L'
033900060523     C                   EVAL      VABCCM = 0592967
034000060523     C                   ENDIF
034100060523     C                   IF        wIOE = 'E'
034200060523     C                   EVAL      VABCCM = 0590502
034300060523     C                   ENDIF
034400060523     C                   ENDIF
034500040707     C*
034600040707     C* Considerazioni particolari sulla tariffa ITALIA o ESTERO
034700040707     C                   IF        wIOE = 'E'
034800040707     C                   EVAL      VABCTR = 099
034900040707     C                   ENDIF
035000060523     C*
035100040706     C                   IF        VABCCM = *zeros
035200040706     C                   SETON                                        31
035300040706     C                   ENDIF
035400010607     C*
035500040706     C* Reperimento campi NUMERICI
035600020722     C                   MOVEL     DATCOR        VABAAS
035700020722     C                   MOVE      DATCOR        VABMGS
035800040420     C* NSP/RMN
035900040707     C                   Z-ADD     *zeros        wPOS              2 0
036000040707     C                   EVAL      wPOS=%scan('/':%subst(vindta:23:10))
036100040707     C                   EVAL      PiStr=%trim(%subst(vindta:23+wPOS:10-wPOS))
036200020304     C                   EXSR      CHKNUM
036300020304     C                   IF        PiInt=*on
036400040301     C                   Z-ADD     PiVal         VABNSP
036500040301     C                   Z-ADD     PiVal         VABRMN
036600020304     C                   ELSE
036700020722     C                   SETON                                        31
036800020304     C                   Z-ADD     *zeros        VABNSP
036900040301     C                   Z-ADD     *zeros        VABRMN
037000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037100040301     C                             + ' ' + 'VABNSP VABRMN'
037200020304     C                   ENDIF
037300040420     C* CAD
037400040706     C                   EVAL      PiStr=%trim(%subst(vindta:295:5))
037500010607     C                   EXSR      CHKNUM
037600010607     C                   IF        PiInt=*on
037700020204     C                   Z-ADD     PiVal         Num5_0
037800020204     C                   MOVEL(p)  Num5_0        VABCAD
037900010607     C                   ELSE
038000010607     C                   SETON                                        32
038100020204     C                   EVAL      VABCAD = *zeros
038200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038300020204     C                             + ' ' + 'VABCAD'
038400010607     C                   ENDIF
038500040420     C* NCL
038600040706     C                   EVAL      PiStr=%trim(%subst(vindta:357:5))
038700010607     C                   EXSR      CHKNUM
038800010607     C                   IF        PiInt=*on
038900010607     C                   Z-ADD     PiVal         VABNCL
039000010607     C                   ELSE
039100010607     C                   SETON                                        32
039200010607     C                   Z-ADD     *zeros        VABNCL
039300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039400010607     C                             + ' ' + 'VABNCL'
039500010607     C                   ENDIF
039600040420     C* PKB
039700040706     C                   EVAL      PiStr=%trim(%subst(vindta:362:10))
039800010607     C                   EXSR      CHKNUM
039900010607     C                   IF        PiNum=*on
040000040420     C                   Z-ADD     PiVal         VABPKB
040100040608     C                   EVAL(H)   VABPKB = PiVal/1000                          * gestisco 3 dec.
040200010607     C                   ELSE
040300010607     C                   SETON                                        32
040400010607     C                   Z-ADD     *zeros        VABPKB
040500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040600010607     C                             + ' ' + 'VABPKB'
040700010607     C                   ENDIF
040800040706     C* VLB
040900040706     C                   EVAL      PiStr=%trim(%subst(vindta:372:10))
041000040706     C                   EXSR      CHKNUM
041100040706     C                   IF        PiNum=*on
041200040706     C                   Z-ADD     PiVal         VABVLB
041300040706     C                   EVAL(H)   VABVLB = PiVal/1000                          * gestisco 3 dec.
041400040706     C                   ELSE
041500040706     C                   SETON                                        32
041600040706     C                   Z-ADD     *zeros        VABVLB
041700040706     C                   EVAL      vinmsg = %trimr(vinmsg)
041800040706     C                             + ' ' + 'VABVLB'
041900040706     C                   ENDIF
042000020322     C*
042100020322     C* Considerazioni finali su CBO/CAS
042200020322     C                   IF        FlgCAS = '1'
042300040706     C                   IF        VABCBO = '1'
042400020322     C                   EVAL      VABCBO = '4'
042500040706     C                   ENDIF
042600040706     C                   IF        VABCBO = '2'
042700040706     C                   EVAL      VABCBO = '6'
042800020322     C                   ENDIF
042900040706     C                   ENDIF
043000020204     C*
043100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043200020204     C                   EXSR      CHKIMPDIV
043300020204     C*
043400010607     C                   ENDSR
043500010607     C*----------------------------------------------------*
043600010601
043700020204
043800020204     C*----------------------------------------------------*
043900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044000020204     C*----------------------------------------------------*
044100020204     C     CHKIMPDIV     BEGSR
044200020204     C*
044300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044400020204     C                   Z-ADD     *zeros        wrkDec            9 9
044500020204     C*
044600020204     C* Come prima cosa effettuo considerazioni sulla divisa
044700020204     C                   IF        vabIAS > *zeros
044800020204     C                   IF        vabVAS <> 'EUR'
044900020204     C                   EVAL      vabVAS =  'ITL'
045000020204     C                   ENDIF
045100020204     C                   ENDIF
045200020204     C*
045300020204     C                   IF        vabCAS > *zeros
045400020204     C                   IF        vabVCA <> 'EUR'
045500020204     C                   EVAL      vabVCA =  'ITL'
045600020204     C                   ENDIF
045700020204     C                   ENDIF
045800020204     C*
045900020204     C                   IF        vabVMD > *zeros
046000020321     C                   IF        vabVAD <> 'EUR'
046100020204     C                   EVAL      vabVAD =  'ITL'
046200020204     C                   ENDIF
046300020204     C                   ENDIF
046400020204     C*
046500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046600020204     C                   Z-ADD     vabIAS        wrkDec
046700020204     C                   IF        wrkDec > *zeros
046800020204     C                   IF        vabVAS = 'ITL'
046900020204     C                   EVAL      vabIAS = *zeros
047000020204     C                   ENDIF
047100020204     C                   ENDIF
047200020204     C*
047300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047400020204     C                   Z-ADD     vabCAS        wrkDec
047500020204     C                   IF        wrkDec > *zeros
047600020204     C                   IF        vabVCA = 'ITL'
047700020204     C                   EVAL      vabCAS = *zeros
047800020204     C                   ENDIF
047900020204     C                   ENDIF
048000020204     C*
048100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048200020204     C                   Z-ADD     vabVMD        wrkDec
048300020204     C                   IF        wrkDec > *zeros
048400020204     C                   IF        vabVAD = 'ITL'
048500020204     C                   EVAL      vabVMD = *zeros
048600020204     C                   ENDIF
048700020204     C                   ENDIF
048800020204     C*
048900020204     C                   ENDSR
049000020204     C***
049100020204
049200010330
049300010330
049400010330     C*----------------------------------------------------*
049500010330     C*  CONTROLLO NUMERICITA' CAMPI
049600010330     C*----------------------------------------------------*
049700010330     C     CHKNUM        BEGSR
049800010330     C*
049900010606     C                   IF        PiDecChr = *blanks
050000010606     C                   EVAL      PiDecChr = ','
050100010606     C                   ENDIF
050200010606     C*
050300010606     C                   CALL(e)   'ISNUMERIC'
050400010330     C                   PARM                    PiStr            30
050500010606     C                   PARM                    PiDecChr          1
050600010330     C                   PARM      *ZEROS        PiVal            30 9
050700010330     C                   PARM      '0'           PiInt             1
050800010330     C                   PARM      '0'           PiNum             1
050900010330     C                   IF        %error
051000010606     C                   EVAL      PiNum=*off
051100010330     C                   ENDIF
051200010330     C*
051300010330     C                   ENDSR
051400010330     C***
051500010330
051600010601
051700010601
051800010601
051900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052000010601     C     repfil        BEGSR
052100010601     C*
052200010601     C                   if        invfil = *zeros and
052300010601     C                             depfil > *zeros and
052400010629     C                             (vinflg = *blanks or
052500010629     C                              vinflg = *zeros)
052600010601     C
052700010601     C                   eval      invfil = depfil
052800010601     C                   endif
052900010601     C*
053000010601     C                   if        depfil <> invfil and
053100010601     C                             invfil > *zeros
053200010601     C                   eval      flgMulti = '1'
053300010601     C                   if        vinflg = *blanks
053400010601     C                   add       1             cntNonEl
053500010601     C                   endif
053600010601     C                   endif
053700010601     C*
053800010601     C                   if        vinflg = '2'
053900010601     C                   eval      flgStato = '2'
054000010601     C                   endif
054100010601     C*
054200010601     C                   ENDSR
054300010601     C***
054400010601
054500010601
054600010601
054700010330
054800010330
054900010330
055000990920      /TITLE Invio dei dati al punto operativo.
055100000613     C     invio         BEGSR
055200990920     C*
055300990920     C                   reset                   dscmz
055400010601     C                   move      invfil        cmzdst
055500990920     C                   eval      cmzfld = vlrfou
055600990920     C                   eval      cmzmbd = vlrhdl
055700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055800000710     C                   if        prmfir = *blanks
055900021014     C                   eval      cmzfla = 'FIVAB00F'
056000021014     C                   eval      cmzmba = 'FIVAB00F'
056100000710     C                   else
056200000710     C                   eval      cmzfla = prmfir
056300000710     C                   eval      cmzmba = prmfir
056400000710     C                   endif
056500990920     C                   eval      cmznrr = *zeros
056600990920     C                   move      §ctrok        cmznrr
056700021018     C                   eval      cmzlba = vlrfl1
056800990920     C                   call(e)   'TIS711C'
056900990920     C                   parm                    dscmz
057000990921     C                   parm      *blanks       esito
057100990920     C                   if        %error
057200990920     C                             or cmzerr = '1'
057300990921     C                             or esito  = '1'
057400000710     C                   eval      wrkesito = '3'
057500990920     C                   endif
057600990920     C*
057700000613     C                   ENDSR
057800990910
057900010601
058000010601
058100010601
058200010601
058300010601      /TITLE Invio dei dati al punto operativo.
058400010601     C     opeini        BEGSR
058500010601     C*
058600010601     C* Inizializzo flag e contatori operativi
058700010601     C                   movel     '0'           flgGiro           1
058800010601     C                   movel     '0'           flgMulti          1
058900010601     C                   movel     '1'           flgStato          1
059000010615     C                   movel     '0'           flgOk             1
059100010601     C                   z-add     *zeros        cntNonEl         10 0
059200010601     C                   z-add     *zeros        depfil            3 0
059300010601     C                   z-add     *zeros        invfil            3 0
059400010601     C*
059500010601     C                   ENDSR
059600010601     C***
059700010601
059800010601
059900010601
060000010330
060100010330
060200000613     C     *inzsr        BEGSR
060300990910     C*
060400990910     C     *entry        plist
060500990920     C                   parm                    tivlrds
060600990921     C                   parm      wrkesito      esito
060700000724     C                   parm                    prmlit
060800000710     C                   parm                    prmfir
060900010330     C*
061000010330     C* CALCOLA LA DATA CORRENTE
061100010330     C                   time                    wn14             14 0
061200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061300010330     C                   z-add     wn8           g08dat
061400010330     C                   z-add     *zeros        g08inv
061500010330     C                   movel     '0'           g08err
061600010330     C                   call      'XSRDA8'
061700010330     C                   parm                    wlbda8
061800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061900000613     C*
062000000613     C                   ENDSR
062100000613     C***
