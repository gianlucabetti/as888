000100171003      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300171003      *
000400171003      * PARTICOLARITA':
000500171003      *
000600171018      * - copiato da TITV1E6R3 del cliente Brooks
000700171018      *
000800130208     Ftabel00f  if   e           k disk
000900990910     Ftivin00r  uF   E             DISK    usropn
001000121005     FEDIVABWR  O    E             DISK    usropn
001100171003     FEDIVATwr  O    E             DISK    usropn
001200000313     D*
001300010330     D*----------------------------------------------------
001400010330     D* DICHIARAZIOINE VARIABILI DI WRK
001500010330     D*----------------------------------------------------
001600010330     D dscmz         e ds                  inz
001700010330     D psds           sds
001800010330     D  procname         *PROC
001900010330     D tivlrds       e ds                  extname(tivlr00f)
002000130208     D ds15          e ds
002100070502     D tisi95ds      e ds
002200010330     D esito           s              1
002300010330     D prmlit          s             10
002400010330     D prmfir          s             10
002500010330     D wrkesito        s                   like(esito)
002600010330     D rrnum           s              6  0 INZ(*zeros)
002700130208     D jNAZ            s              5  0 INZ(*zeros)
002800171003     D parccm          s              8    INZ(*blanks)
002900171003     D parmbr          s             10    INZ(*blanks)
003000171003     D paropz          s              1    INZ(*blanks)
003100171003     D chkcall         s              1    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000130208     D*------------
004100130208     D skNAZISO        S              3    DIM(1000)
004200130208     D skNAZBAR        S              3    DIM(1000)
004300010330
004400010330
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900130208     C                   EXSR      CARTAB                                       CARICA TABELLE
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200171003     C*
005300171003     C* Effettuo la chiamata al CLLE preposto
005400171003     C                   call(e)   'TITVEVTC'
005500171003     C                   parm                    parccm
005600171003     C                   parm                    parmbr
005700171003     C                   parm      '2'           paropz
005800070502     C*
005900070502     C* Effettuo lancio TISI95 solo x chiusura
006000070502     C                   CLEAR                   TISI95DS
006100070502     C                   EVAL      I95TLA = 'C'
006200070502     C                   CALL      'TISI95R'
006300070502     C                   PARM                    TISI95DS
006400010601     C*
006500010601     C                   seton                                        lr
006600010601
006700010601
006800010601
006900010601
007000010601     C*--------------------------------------------------------
007100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007200010601     C*--------------------------------------------------------
007300010601     C     PREELA        BEGSR
007400010601     C*
007500010601     C* SE OCCORRE SPEDIRE IN FILIALE
007600010601     C                   if        invfil <> *zeros and
007700010601     C                             flgGiro = '0'
007800010601     C*
007900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008000010601     C                   eval      flgGiro = '1'
008100010601     C*
008200010601     C                   endif
008300010601     C*
008400010601     C                   ENDSR
008500010601     C***
008600010601
008700010601
008800010601
008900010601     C*--------------------------------------------------------
009000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009100010601     C*--------------------------------------------------------
009200010601     C     ENDELA        BEGSR
009300000616     C*
009400010601     C                   ENDSR
009500010601     C***
009600000613
009700010601
009800010601
009900010330     C*--------------------------------------------------------
010000171003     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR/EDIVATWR     *
010100010330     C*--------------------------------------------------------
010200010605     C     RWVAB         BEGSR
010300050408     C*
010400010330     C                   if        not %open(tivin00r)
010500010330     C                   open      tivin00r
010600010330     C                   endif
010700121005     C                   if        not %open(edivabwr)
010800121005     C                   open      edivabwr
010900010330     C                   endif
011000171003     C*
011100171003     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011200171003     C                   exsr      prevat
011300171003     C*
011400171003     C                   if        chkcall = '0'
011500171003     C*
011600171003     C                   if        not %open(edivatwr)
011700171003     C                   open      edivatwr
011800171003     C                   endif
011900010330     C*
012000171003     C                   clear                   §CTROKVB
012100171003     C                   clear                   §CTROKVT          5 0
012200010604     C                   clear                   §CTRMO
012300010604     C                   clear                   §CTRNO
012400010330     C*
012500010330     C                   DO        *HIVAL
012600010330     C*
012700010330     C                   READ      tivin00r                               70
012800010618     C*
012900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013000010618     C                   if        vinflg = '1'
013100010618     C                   eval      flgOk = '1'
013200010618     C                   endif
013300010618     C*
013400010330     C                   if        vindta > *blanks
013500010330     C                   add       1             rrnum
013600010330     C*
013700010601     C                   if        *in70 = *off and
013800010330     C                             (vinflg = *blanks
013900010330     C                              or vinflg = '0'
014000010330     C                              or vinflg = '2')
014100010330     C*
014200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014300010711     C                   if        vinflg = *blanks or vinflg = '0'
014400010711     C                   clear                   vinmsg
014500010711     C                   endif
014600070601     C*
014700070601     C                   exsr      inzvar
014800070601     C                   exsr      defcam
014900070601     C                   exsr      impvab
015000010601     C*
015100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
015200010605     C                   eval      depfil = VABLNP
015300010601     C                   exsr      repfil
015400010601     C                   if        depfil = invfil
015500021025     C                   if        vlrpoi = 999
015600021025     C                   MOVE(P)   invfil        VABFGS
015700021025     C                   else
015800021025     C                   MOVE(P)   vlrpoi        VABFGS
015900021025     C                   endif
016000171003     C                   EVAL      VATFGS = VABFGS
016100171003     C*
016200171003     C* Scrivo sempre le estensioni del record corrente
016300171023     C                   exsr      exevatB                                      => write VAT
016400010601     C*
016500171023     C                   exsr      exevatI                                      => write VAT
016600171023     C*
016700010601     C                   exsr      PREELA
016800010601     C*
016900010604     C* Ebbene...
017000010604     C*
017100171003     C  N31              ADD       1             §CTROKVB          7 0
017200010604     C   32              ADD       1             §CTRMO            7 0
017300010604     C   31              ADD       1             §CTRNO            7 0
017400010604     C*
017500071120     C                   exsr      wrivab                                       => scrivo VAB
017600071120     C*
017700020722     C                   endif
017800020722     C*
017900010604     C                   if        *in31 = *off and
018000010604     C                             *in32 = *off
018100010604     C                   eval      vinflg = '1'
018200010604     C                   else
018300010604     C                   eval      vinflg = '2'
018400010604     C                   endif
018500010604     C                   endif
018600010604     C*
018700010330     C                   else
018800010330     C                   eval      vinflg = '1'
018900010330     C                   endif
019000010601     C*
019100010601     C  N70              update    tivin000
019200010330     C*
019300010330     C  N70              ENDdo
019400171003     C*
019500171003     C                   endif
019600010601     C*
019700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019800010601     C                   if        cntNonEl = *zeros or
019900010601     C                             flgMulti = '0'
020000010330     C* Se non ci sono record con errori ...
020100010601     C                   if        §ctrno = 0 and
020200010604     C                             §ctrmo = 0 and
020300010601     C                             flgStato <> '2'
020400010330     C* ... restituisco esito OK.
020500010330     C                   eval      wrkesito = '0'
020600010330     C                   else
020700171003     C                   if        §ctrokvb > 0
020800010330     C                   eval      wrkesito = '1'
020900010330     C                   else
021000010615     C                   if        flgOk = '0'
021100010615     C                   eval      wrkesito = '2'
021200010615     C                   else
021300010615     C                   eval      wrkesito = '6'
021400010615     C                   endif
021500010330     C                   endif
021600010330     C                   endif
021700010601     C                   else
021800010601     C                   eval      wrkesito = '9'
021900010601     C                   endif
022000010330     C*
022100010330     C                   if        %open(tivin00r)
022200010330     C                   close     tivin00r
022300010330     C                   endif
022400121005     C                   if        %open(edivabwr)
022500121005     C                   close     edivabwr
022600010330     C                   endif
022700171003     C                   if        %open(edivatwr)
022800171003     C                   close     edivatwr
022900171003     C                   endif
023000010601     C*
023100010601     C                   if        vlrpoi <> 999
023200010601     C                   eval      invfil = vlrpoi
023300010601     C                   endif
023400010330     C*
023500171003     C                   if        §ctrokvb > 0
023600010601     C                             and invfil > *zeros
023700010330     C                   exsr      invio
023800010330     C                   endif
023900010601     C*
024000010618     C                   if        flgGiro = '1'
024100010601     C                   exsr      endela
024200010618     C                   endif
024300010330     C*
024400010330     C                   ENDSR
024500010330     C***
024600070601
024700070601     C*----------------------------------------------------*
024800070601     C*  SCARICAMENTO BUFFER RECORDS VAB
024900070601     C*----------------------------------------------------*
025000070601     C     WRIVAB        BEGSR
025100171003     C*
025200121005     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
025300121015     C                   EVAL      VABCMR = %char(datcor)
025400121005     C                   EVAL      VABDCM = datcor
025500121005     C                   EVAL      VABDTS = datcor
025600121005     C                   EVAL      VABHMS = oracor
025700121005     C                   EVAL      VABCNT = 1
025800070601     C*
025900121005     C  N31              WRITE     EDIVAB00
026000070601     C*
026100070601     C                   ENDSR
026200010601
026300010601
026400010601
026500010330     C*----------------------------------------------------*
026600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026700010330     C*----------------------------------------------------*
026800010330     C     INZVAR        BEGSR
026900171003     C*
027000171003     C* Inizializzo il buffer del record da scrivere
027100171003     C                   CLEAR                   EDIVAB00
027200171003     C                   CLEAR                   EDIVAT00
027300010330     C*
027400020204     C                   Z-ADD     *zeros        Num5_0            5 0
027500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027600020322     C                   MOVEL     '0'           FlgCAS            1
027700010330     C*
027800010330     C                   ENDSR
027900010330     C*----------------------------------------------------*
028000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
028100010330     C*----------------------------------------------------*
028200010330     C     DEFCAM        BEGSR
028300010330     C*
028400020204     C* Imposto i valori di default...
028500171023     C                   EVAL      VABCCM = 0204292
028600171023     C                   EVAL      VATCCM = 0204292
028700171023     C                   EVAL      VABLNP = 020
028800171023     C                   EVAL      VATLNP = 020
028900171018     C                   EVAL      VABCTR = 300
029000080311     C                   EVAL      VABCTM = '7Q'
029100070208     C                   EVAL      VABCBO = '1'
029200020204     C* ... e poi verifico se sono stati passati come parametri
029300020204     C                   IF        vlrppt > *blanks
029400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
029500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600020204     C                   EXSR      CHKNUM
029700020204     C                   IF        PiInt=*on
029800020204     C                   Z-ADD     PiVal         VABCCM
029900171003     C                   Z-ADD     PiVal         VATCCM
030000020204     C                   ENDIF
030100040301     C                   ENDIF
030200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
030300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400020204     C                   EXSR      CHKNUM
030500020204     C                   IF        PiInt=*on
030600020204     C                   Z-ADD     PiVal         VABLNP
030700171003     C                   Z-ADD     PiVal         VATLNP
030800020204     C                   ENDIF
030900040301     C                   ENDIF
031000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
031100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400020204     C                   Z-ADD     PiVal         VABCTR
031500040301     C                   ENDIF
031600020204     C                   ENDIF
031700020204     C                   ENDIF
031800020204     C*
031900010330     C                   ENDSR
032000010607     C*----------------------------------------------------*
032100121005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
032200010607     C*----------------------------------------------------*
032300010607     C     IMPVAB        BEGSR
032400010607     C*
032500010607     C                   SETOFF                                       3132
032600010607     C*
032700010607     C* Reperimento campi ALFA
032800010607     C*
032900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
033000070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
033100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033300020117     C* ==
033400171018     C***                EVAL      VABRD2=%trim(%subst(vindta:407:20))
033500070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
033600070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
033700070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
033800130208     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
033900130208     C                   Z-ADD     1             jNAZ
034000130208     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
034100130208     C                   IF        %found
034200130208     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
034300130208     C                   ENDIF
034400171018     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
034500080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
034600080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
034700171018     C***                IF        %subst(vindta:210:1) = 'M'
034800171018     C***                EVAL      VABTIC='BM'
034900171018     C***                ENDIF
035000171023     C***                IF        %subst(vindta:210:1) = 'N'
035100171023     C***                EVAL      VABTIC='TM'
035200171023     C***                ENDIF
035300171018     C***                IF        %subst(vindta:210:1) = 'R'
035400171018     C***                EVAL      VABTIC='CM'
035500171018     C***                ENDIF
035600060510     C*
035700060510     C* Reperimento campi NUMERICI
035800060510     C                   MOVEL     DATCOR        VABAAS
035900060510     C                   MOVE      DATCOR        VABMGS
036000171003     C                   MOVEL     DATCOR        VATAAS
036100060710     C* NSP/RMN
036200080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
036300060510     C                   EXSR      CHKNUM
036400060510     C                   IF        PiInt=*on
036500060510     C                   Z-ADD     PiVal         VABNSP
036600171003     C                   Z-ADD     PiVal         VATNSP
036700060510     C                   Z-ADD     PiVal         VABRMN
036800060404     C                   ELSE
036900070502     C                   SETON                                        31
037000060510     C                   Z-ADD     *zeros        VABNSP
037100171003     C                   Z-ADD     *zeros        VATNSP
037200060510     C                   Z-ADD     1             VABRMN
037300060404     C                   EVAL      vinmsg = %trimr(vinmsg)
037400060510     C                             + ' ' + 'VABNSP VABRMN'
037500060404     C                   ENDIF
037600040420     C* CAD
037700171018     C***                EVAL      VABCAD=%trim(%subst(vindta:145:9))
037800171018     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
037900171018     C                   EXSR      CHKNUM
038000171018     C                   IF        PiInt=*on
038100171018     C                   Z-ADD     PiVal         Num5_0
038200171018     C                   MOVEL(p)  Num5_0        VABCAD
038300171018     C                   ELSE
038400171018     C                   SETON                                        32
038500171018     C                   EVAL      VABCAD = *zeros
038600171018     C                   EVAL      vinmsg = %trimr(vinmsg)
038700171018     C                             + ' ' + 'VABCAD'
038800171018     C                   ENDIF
038900070502     C* Reperisco la provincia dal CAP e dalla località
039000170223     C                   IF        VABNZD  = *blanks AND
039100170223     C                             VABCAD <> *blanks AND
039200070502     C                             VABPRD  = *blanks
039300070502     C                   CLEAR                   TISI95DS
039400070502     C                   EVAL      I95TCN = '3'
039500070502     C                   Z-ADD     datcor        I95DAT
039600070502     C                   EVAL      I95CAP = VABCAD
039700070502     C                   EVAL      I95LOC = VABLOD
039800070502     C                   EVAL      I95NAR = VABNZD
039900070502     C                   CALL      'TISI95R'
040000070502     C                   PARM                    TISI95DS
040100070502     C                   EVAL      VABPRD = O95PRV
040200070502     C                   ENDIF
040300040420     C* NCL
040400070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
040500010607     C                   EXSR      CHKNUM
040600010607     C                   IF        PiInt=*on
040700070606     C                   Z-ADD     PiVal         VABNCL
040800010607     C                   ELSE
040900010607     C                   SETON                                        32
041000010607     C                   Z-ADD     *zeros        VABNCL
041100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041200010607     C                             + ' ' + 'VABNCL'
041300010607     C                   ENDIF
041400040420     C* PKB
041500070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
041600070208     C                   EVAL      PiDecChr = '.'
041700010607     C                   EXSR      CHKNUM
041800010607     C                   IF        PiNum=*on
041900070925     C                   Z-ADD(H)  PiVal         VABPKB
042000010607     C                   ELSE
042100010607     C                   SETON                                        32
042200010607     C                   Z-ADD     *zeros        VABPKB
042300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042400010607     C                             + ' ' + 'VABPKB'
042500010607     C                   ENDIF
042600070925     C* VLB
042700171018     C* passato sempre a 0
042800070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
042900070925     C                   EVAL      PiDecChr = '.'
043000070925     C                   EXSR      CHKNUM
043100070925     C                   IF        PiNum=*on
043200070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
043300070925     C                   Z-ADD     PiVal         VABVLB
043400070925     C                   ELSE
043500070925     C                   SETON                                        32
043600070925     C                   Z-ADD     *zeros        VABVLB
043700070925     C                   EVAL      vinmsg = %trimr(vinmsg)
043800070925     C                             + ' ' + 'VABVLB'
043900070925     C                   ENDIF
044000080311     C* CAS
044100080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
044200080311     C                             %subst(vindta:196:14) <> *zeros
044300080311     C                   EVAL      FlgCAS = '1'
044400080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
044500080311     C                   EVAL      PiDecChr = '.'
044600080311     C                   EXSR      CHKNUM
044700080311     C                   IF        PiNum=*on
044800080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
044900080311     C                   Z-ADD     PiVal         VABCAS
045000171109     C                   EVAL      VABVCA = 'EUR'
045100080311     C                   ELSE
045200080311     C                   SETON                                        32
045300120123     C                   Z-ADD     *zeros        VABCAS
045400080311     C                   EVAL      vinmsg = %trimr(vinmsg)
045500080311     C                             + ' ' + 'VABCAS'
045600080311     C                   ENDIF
045700080311     C                   ENDIF
045800020322     C*
045900020322     C* Considerazioni finali su CBO/CAS
046000050908     C                   IF        FlgCAS = '1'
046100040713     C                   IF        VABCBO = '1'
046200020322     C                   EVAL      VABCBO = '4'
046300040713     C                   ENDIF
046400040713     C                   IF        VABCBO = '2'
046500040713     C                   EVAL      VABCBO = '6'
046600040713     C                   ENDIF
046700020322     C                   ENDIF
046800020204     C*
046900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047000020204     C                   EXSR      CHKIMPDIV
047100020204     C*
047200010607     C                   ENDSR
047300010607     C*----------------------------------------------------*
047400171023     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
047500171023     C*----------------------------------------------------*
047600171023     C     EXEVATB       BEGSR
047700171023     C*
047800171023     C                   IF        %subst(vindta:407:20) <> *blank
047900171023     C                   EVAL      VATTRC='B'
048000171023     C                   EVAL      VATNOT = %trim(%subst(vindta:407:20))
048100171023     C                   exsr      wrivat                                       => scarico VAT
048200171023     C                   endif
048300171023     C*
048400171023     C                   ENDSR
048500171003     C*----------------------------------------------------*
048600171003     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
048700171003     C*----------------------------------------------------*
048800171003     C     EXEVATI       BEGSR
048900171003     C*
049000171023     C                   IF        %subst(vindta:502:35) <> *blank
049100171003     C                   EVAL      VATTRC='I'
049200171023     C                   EVAL      VATNOT = %trim(%subst(vindta:502:35))
049300171003     C                   exsr      wrivat                                       => scarico VAT
049400171003     C                   endif
049500171003     C*
049600171023     C                   IF        %subst(vindta:502+35:15) <> *blank
049700171005     C                   EVAL      VATTRC='J'
049800171023     C                   EVAL      VATNOT = %trim(%subst(vindta:502+35:15))
049900171003     C                   exsr      wrivat                                       => scarico VAT
050000171003     C                   endif
050100171003     C*
050200171003     C                   ENDSR
050300171003     C*----------------------------------------------------*
050400171003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
050500171003     C*----------------------------------------------------*
050600171003     C     WRIVAT        BEGSR
050700171003     C*
050800171003     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
050900171003     C                   EVAL      VATCMR = %char(datcor)
051000171003     C                   EVAL      VATCNT = 1
051100171003     C*
051200171003     C* Scrivo solo se valorizzato qualcosa
051300171003     C                   IF        VATNOT <> *blanks
051400171003     C                   WRITE     EDIVAT00
051500171003     C                   ENDIF
051600171003     C*
051700171003     C                   ENDSR
051800010601
051900020204
052000020204     C*----------------------------------------------------*
052100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052200020204     C*----------------------------------------------------*
052300020204     C     CHKIMPDIV     BEGSR
052400020204     C*
052500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052600020204     C                   Z-ADD     *zeros        wrkDec            9 9
052700020204     C*
052800020204     C* Come prima cosa effettuo considerazioni sulla divisa
052900020204     C                   IF        vabIAS > *zeros
053000020204     C                   IF        vabVAS <> 'EUR'
053100020204     C                   EVAL      vabVAS =  'ITL'
053200020204     C                   ENDIF
053300020204     C                   ENDIF
053400020204     C*
053500020204     C                   IF        vabCAS > *zeros
053600020204     C                   IF        vabVCA <> 'EUR'
053700020204     C                   EVAL      vabVCA =  'ITL'
053800020204     C                   ENDIF
053900020204     C                   ENDIF
054000020204     C*
054100020204     C                   IF        vabVMD > *zeros
054200020321     C                   IF        vabVAD <> 'EUR'
054300020204     C                   EVAL      vabVAD =  'ITL'
054400020204     C                   ENDIF
054500020204     C                   ENDIF
054600020204     C*
054700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054800020204     C                   Z-ADD     vabIAS        wrkDec
054900020204     C                   IF        wrkDec > *zeros
055000020204     C                   IF        vabVAS = 'ITL'
055100020204     C                   EVAL      vabIAS = *zeros
055200020204     C                   ENDIF
055300020204     C                   ENDIF
055400020204     C*
055500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
055600020204     C                   Z-ADD     vabCAS        wrkDec
055700020204     C                   IF        wrkDec > *zeros
055800020204     C                   IF        vabVCA = 'ITL'
055900020204     C                   EVAL      vabCAS = *zeros
056000020204     C                   ENDIF
056100020204     C                   ENDIF
056200020204     C*
056300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056400020204     C                   Z-ADD     vabVMD        wrkDec
056500020204     C                   IF        wrkDec > *zeros
056600020204     C                   IF        vabVAD = 'ITL'
056700020204     C                   EVAL      vabVMD = *zeros
056800020204     C                   ENDIF
056900020204     C                   ENDIF
057000020204     C*
057100020204     C                   ENDSR
057200020204     C***
057300020204
057400010330
057500010330
057600171003     C*----------------------------------------------------*
057700171003     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
057800171003     C*----------------------------------------------------*
057900171003     C     PREVAT        BEGSR
058000171003     C*
058100171003     C* Compongo il nome del membro da dare al EDIVATWR
058200171003     C                   eval      parmbr = vlrhdl
058300171003     C                   movel     'M'           parmbr
058400171003     C                   eval      parccm = %subst(vlrKSC:2:7)
058500171003     C                   eval      paropz = '1'
058600171003     C* Effettuo la chiamata al CLLE preposto
058700171003     C                   call(e)   'TITVEVTC'
058800171003     C                   parm                    parccm
058900171003     C                   parm                    parmbr
059000171003     C                   parm                    paropz
059100171003     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059200171003     C                   if        %error
059300171003     C                   movel     '1'           chkcall
059400171003     C                   else
059500171003     C                   movel     '0'           chkcall
059600171003     C                   endif
059700171003     C*
059800171003     C                   ENDSR
059900010330     C*----------------------------------------------------*
060000010330     C*  CONTROLLO NUMERICITA' CAMPI
060100010330     C*----------------------------------------------------*
060200010330     C     CHKNUM        BEGSR
060300010330     C*
060400010606     C                   IF        PiDecChr = *blanks
060500070502     C                   EVAL      PiDecChr = '.'
060600010606     C                   ENDIF
060700010606     C*
060800010606     C                   CALL(e)   'ISNUMERIC'
060900010330     C                   PARM                    PiStr            30
061000010606     C                   PARM                    PiDecChr          1
061100010330     C                   PARM      *ZEROS        PiVal            30 9
061200010330     C                   PARM      '0'           PiInt             1
061300010330     C                   PARM      '0'           PiNum             1
061400010330     C                   IF        %error
061500010606     C                   EVAL      PiNum=*off
061600010330     C                   ENDIF
061700010330     C*
061800010330     C                   ENDSR
061900010330     C***
062000010330
062100010601
062200010601
062300010601
062400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
062500010601     C     repfil        BEGSR
062600010601     C*
062700010601     C                   if        invfil = *zeros and
062800010601     C                             depfil > *zeros and
062900010629     C                             (vinflg = *blanks or
063000010629     C                              vinflg = *zeros)
063100010601     C
063200010601     C                   eval      invfil = depfil
063300010601     C                   endif
063400010601     C*
063500010601     C                   if        depfil <> invfil and
063600010601     C                             invfil > *zeros
063700010601     C                   eval      flgMulti = '1'
063800010601     C                   if        vinflg = *blanks
063900010601     C                   add       1             cntNonEl
064000010601     C                   endif
064100010601     C                   endif
064200010601     C*
064300010601     C                   if        vinflg = '2'
064400010601     C                   eval      flgStato = '2'
064500010601     C                   endif
064600010601     C*
064700010601     C                   ENDSR
064800010601     C***
064900010601
065000010601
065100010601
065200010330
065300010330
065400010330
065500990920      /TITLE Invio dei dati al punto operativo.
065600000613     C     invio         BEGSR
065700171003     C*
065800171003     C* 1° invio EDIVAT
065900171003     C                   reset                   dscmz
066000171003     C                   move      vlrpoi        cmzdst
066100171003     C                   eval      cmzfld = 'EDIVATWR'
066200171003     C                   eval      cmzmbd = vlrhdl
066300171003     C                   eval      %subst(cmzmbd:1:1) = 'M'
066400171003     C***                if        prmfir = *blanks
066500171003     C                   eval      cmzfla = 'EDIVAT0F'
066600171003     C                   eval      cmzmba = 'EDIVAT0F'
066700171003     C***                else
066800171003     C***                eval      cmzfla = prmfir
066900171003     C***                eval      cmzmba = prmfir
067000171003     C***                endif
067100171003     C                   eval      cmznrr = *zeros
067200171003     C                   move      §ctrokvt      cmznrr
067300171003     C                   eval      cmzlba = vlrfl1
067400171003     C                   call(e)   'TIS711C'
067500171003     C                   parm                    dscmz
067600171003     C                   parm      *blanks       esito
067700171003     C                   if        %error
067800171003     C                             or cmzerr = '1'
067900171003     C                             or esito  = '1'
068000171003     C                   eval      wrkesito = '3'
068100171003     C                   else
068200171003     C*
068300171003     C* 2° invio EDIVAB
068400990920     C                   reset                   dscmz
068500010601     C                   move      invfil        cmzdst
068600990920     C                   eval      cmzfld = vlrfou
068700990920     C                   eval      cmzmbd = vlrhdl
068800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
068900171003     C***                if        prmfir = *blanks
069000121005     C                   eval      cmzfla = 'EDIVAB0F'
069100121005     C                   eval      cmzmba = 'EDIVAB0F'
069200171003     C***                else
069300171003     C***                eval      cmzfla = prmfir
069400171003     C***                eval      cmzmba = prmfir
069500171003     C***                endif
069600990920     C                   eval      cmznrr = *zeros
069700171003     C                   move      §ctrokvb      cmznrr
069800021018     C                   eval      cmzlba = vlrfl1
069900990920     C                   call(e)   'TIS711C'
070000990920     C                   parm                    dscmz
070100990921     C                   parm      *blanks       esito
070200990920     C                   if        %error
070300990920     C                             or cmzerr = '1'
070400990921     C                             or esito  = '1'
070500000710     C                   eval      wrkesito = '3'
070600990920     C                   endif
070700171003     C                   endif
070800990920     C*
070900000613     C                   ENDSR
071000130208
071100130208     C*--------------------------------------------------------
071200130208     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
071300130208     C*--------------------------------------------------------
071400130208     C     CARTAB        BEGSR
071500130208     C*
071600130208     C* TABELLA '15' - NAZIONI
071700130208     C                   clear                   skNAZISO
071800130208     C                   clear                   skNAZBAR
071900130208     C                   eval      tblKUT = 1
072000130208     C                   eval      tblCOD = '15'
072100130208     C     KEYtabP       setll     tabel00f
072200130208     C     KEYtabP       reade     tabel00f
072300130208     C                   dow       not %eof(tabel00f)
072400130208     C                   if        tblFLG = *blanks
072500130208     C                   movel(p)  tblUNI        ds15
072600130208     C                   add       1             jNAZ
072700130208     C                   eval      skNAZBAR(jNAZ) = tblKEY
072800130208     C                   eval      skNAZISO(jNAZ) = §15COD
072900130208     C                   endif
073000130208     C     KEYtabP       reade     tabel00f
073100130208     C                   enddo
073200130208     C*
073300130208     C                   ENDSR
073400130208     C***
073500990910
073600010601
073700010601
073800010601
073900010601
074000010601      /TITLE Invio dei dati al punto operativo.
074100010601     C     opeini        BEGSR
074200010601     C*
074300010601     C* Inizializzo flag e contatori operativi
074400010601     C                   movel     '0'           flgGiro           1
074500010601     C                   movel     '0'           flgMulti          1
074600010601     C                   movel     '1'           flgStato          1
074700010615     C                   movel     '0'           flgOk             1
074800010601     C                   z-add     *zeros        cntNonEl         10 0
074900010601     C                   z-add     *zeros        depfil            3 0
075000010601     C                   z-add     *zeros        invfil            3 0
075100010601     C*
075200010601     C                   ENDSR
075300010601     C***
075400010601
075500010601
075600010601
075700010330
075800010330
075900000613     C     *inzsr        BEGSR
076000990910     C*
076100990910     C     *entry        plist
076200990920     C                   parm                    tivlrds
076300990921     C                   parm      wrkesito      esito
076400000724     C                   parm                    prmlit
076500000710     C                   parm                    prmfir
076600010330     C*
076700010330     C* CALCOLA LA DATA CORRENTE
076800010330     C                   time                    wn14             14 0
076900121005     C                   movel     wn14          oracor            6 0          *ORA
077000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
077100010330     C                   z-add     wn8           g08dat
077200010330     C                   z-add     *zeros        g08inv
077300010330     C                   movel     '0'           g08err
077400010330     C                   call      'XSRDA8'
077500010330     C                   parm                    wlbda8
077600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
077700130208     C*
077800130208     C* Chiave su TABEL00F - parziale
077900130208     C     KEYtabP       KLIST
078000130208     C                   KFLD                    tblKUT
078100130208     C                   KFLD                    tblCOD
078200000613     C*
078300000613     C                   ENDSR
078400000613     C***
