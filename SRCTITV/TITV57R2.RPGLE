000100040628      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR. (x MEDIAWORLD - Cod. 0931845)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040628     FFIVATwwr  O    E             DISK    usropn
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
002000040628     D parccm          s              8    INZ(*blanks)
002100040628     D parmbr          s             10    INZ(*blanks)
002200040628     D paropz          s              1    INZ(*blanks)
002300040628     D chkcall         s              1    INZ(*blanks)
002400130521     D wChiSono        s             18    VARYING INZ(*zeros)
002500060823     D wTelDest        s             30    VARYING INZ(*blanks)
002600010330     D*------------------
002700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010330     D*------------------
002900010330     D WLBDA8          DS                  INZ
003000010330     D  G08DAT                 1      8  0
003100010330     D  G08INV                 9     16  0
003200010330     D  G08ERR                17     17
003300010330     D  G08TGI                18     22  0
003400000313     D*
003500010330
003600010330
003700010330
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100010601     C*
004200010601     C                   exsr      opeini
004300010605     C                   exsr      rwvab
004400040628     C*
004500040628     C* Effettuo la chiamata al CLLE preposto
004600040628     C                   call(e)   'TITVVTC'
004700040628     C                   parm                    parccm
004800040628     C                   parm                    parmbr
004900040628     C                   parm      '2'           paropz
005000010601     C*
005100010601     C                   seton                                        lr
005200010601
005300010601
005400010601
005500010601
005600010601     C*--------------------------------------------------------
005700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005800010601     C*--------------------------------------------------------
005900010601     C     PREELA        BEGSR
006000010601     C*
006100010601     C* SE OCCORRE SPEDIRE IN FILIALE
006200010601     C                   if        invfil <> *zeros and
006300010601     C                             flgGiro = '0'
006400010601     C*
006500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006600010601     C                   eval      flgGiro = '1'
006700010601     C*
006800010601     C                   endif
006900010601     C*
007000010601     C                   ENDSR
007100010601     C***
007200010601
007300010601
007400010601
007500010601
007600010601
007700010601
007800010601
007900010601     C*--------------------------------------------------------
008000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008100010601     C*--------------------------------------------------------
008200010601     C     ENDELA        BEGSR
008300000616     C*
008400010601     C                   ENDSR
008500010601     C***
008600000613
008700010330
008800010601
008900010601
009000010601
009100010330     C*--------------------------------------------------------
009200040628     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF e FIVATWWR   *
009300010330     C*--------------------------------------------------------
009400010605     C     RWVAB         BEGSR
009500010330     C*
009600010330     C                   if        not %open(tivin00r)
009700010330     C                   open      tivin00r
009800010330     C                   endif
009900021025     C                   if        not %open(fivabwwr)
010000021025     C                   open      fivabwwr
010100010330     C                   endif
010200040628     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010300040628     C                   exsr      prevat
010400040628     C*
010500040628     C                   if        chkcall = '0'
010600040628     C*
010700040628     C                   if        not %open(fivatwwr)
010800040628     C                   open      fivatwwr
010900040628     C                   endif
011000010330     C*
011100040628     C                   clear                   §CTROKVB
011200040628     C                   clear                   §CTROKVT
011300040628     C                   clear                   §CTRMO
011400010604     C                   clear                   §CTRNO
011500010330     C*
011600010330     C                   DO        *HIVAL
011700010330     C*
011800010330     C                   READ      tivin00r                               70
011900010618     C*
012000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012100010618     C                   if        vinflg = '1'
012200010618     C                   eval      flgOk = '1'
012300010618     C                   endif
012400010618     C*
012500010330     C                   if        vindta > *blanks
012600010330     C                   add       1             rrnum
012700010330     C*
012800010601     C                   if        *in70 = *off and
012900010330     C                             (vinflg = *blanks
013000010330     C                              or vinflg = '0'
013100010330     C                              or vinflg = '2')
013200010330     C*
013300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013400010711     C                   if        vinflg = *blanks or vinflg = '0'
013500010711     C                   clear                   vinmsg
013600010711     C                   endif
013700040628     C*
013800010330     C                   exsr      inzvar
013900010330     C                   exsr      defcam
014000010605     C                   exsr      impvab
014100010601     C*
014200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014300010605     C                   eval      depfil = VABLNP
014400010601     C                   exsr      repfil
014500010601     C                   if        depfil = invfil
014600021025     C                   if        vlrpoi = 999
014700021025     C                   MOVE(P)   invfil        VABFGS
014800021025     C                   else
014900021025     C                   MOVE(P)   vlrpoi        VABFGS
015000021025     C                   endif
015100040628     C*
015200040628     C                   exsr      wrivat
015300010601     C*
015400010601     C                   exsr      PREELA
015500010601     C*
015600010604     C* Ebbene...
015700010604     C*
015800040628     C  N31              ADD       1             §CTROKVB          7 0
015900010604     C   32              ADD       1             §CTRMO            7 0
016000010604     C   31              ADD       1             §CTRNO            7 0
016100021025     C  N31              WRITE     FIVAB000
016200010604     C*
016300010604     C                   if        *in31 = *off and
016400010604     C                             *in32 = *off
016500010604     C                   eval      vinflg = '1'
016600010604     C                   else
016700010604     C                   eval      vinflg = '2'
016800010604     C                   endif
016900010604     C                   endif
017000010601     C*
017100010604     C                   endif
017200010604     C*
017300010330     C                   else
017400010330     C                   eval      vinflg = '1'
017500010330     C                   endif
017600010601     C*
017700010601     C  N70              update    tivin000
017800010330     C*
017900010330     C  N70              ENDdo
018000040628     C*
018100040628     C                   endif
018200010601     C*
018300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018400010601     C                   if        cntNonEl = *zeros or
018500010601     C                             flgMulti = '0'
018600010330     C* Se non ci sono record con errori ...
018700010601     C                   if        §ctrno = 0 and
018800010604     C                             §ctrmo = 0 and
018900010601     C                             flgStato <> '2'
019000010330     C* ... restituisco esito OK.
019100010330     C                   eval      wrkesito = '0'
019200010330     C                   else
019300040628     C                   if        §ctrokvb > 0
019400010330     C                   eval      wrkesito = '1'
019500010330     C                   else
019600010615     C                   if        flgOk = '0'
019700010615     C                   eval      wrkesito = '2'
019800010615     C                   else
019900010615     C                   eval      wrkesito = '6'
020000010615     C                   endif
020100010330     C                   endif
020200010330     C                   endif
020300010601     C                   else
020400010601     C                   eval      wrkesito = '9'
020500010601     C                   endif
020600010330     C*
020700010330     C                   if        %open(tivin00r)
020800010330     C                   close     tivin00r
020900010330     C                   endif
021000021025     C                   if        %open(fivabwwr)
021100021025     C                   close     fivabwwr
021200010330     C                   endif
021300040628     C                   if        %open(fivatwwr)
021400040628     C                   close     fivatwwr
021500040628     C                   endif
021600010601     C*
021700010601     C                   if        vlrpoi <> 999
021800010601     C                   eval      invfil = vlrpoi
021900010601     C                   endif
022000010330     C*
022100040628     C                   if        §ctrokvb > 0
022200010601     C                             and invfil > *zeros
022300010330     C                   exsr      invio
022400010330     C                   endif
022500010601     C*
022600010618     C                   if        flgGiro = '1'
022700010601     C                   exsr      endela
022800010618     C                   endif
022900010330     C*
023000010330     C                   ENDSR
023100010330     C***
023200010601
023300010601
023400010601
023500010330     C*----------------------------------------------------*
023600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023700010330     C*----------------------------------------------------*
023800010330     C     INZVAR        BEGSR
023900010330     C*
024000020204     C                   Z-ADD     *zeros        Num5_0            5 0
024100030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024200030606     C                   MOVEL     '0'           FlgCAS            1
024300010330     C*
024400010330     C                   ENDSR
024500010330     C*----------------------------------------------------*
024600010330     C*  IMPOSTAZIONE CAMPI COSTANTI
024700010330     C*----------------------------------------------------*
024800010330     C     DEFCAM        BEGSR
024900010330     C*
025000020204     C* Inizializzo il buffer del record da scrivere
025100021025     C                   CLEAR                   FIVAB000
025200040628     C                   CLEAR                   FIVAT000
025300020204     C* Imposto i valori di default...
025400030606     C                   EVAL      VABCCM = 0931845
025500030606     C                   EVAL      VABLNP = 093
025600030526     C                   EVAL      VABCTR = 000
025700040628     C                   EVAL      VABCTM = '7Q'
025800080618     C                   EVAL      VABTSP = 'C'
025900020204     C* ... e poi verifico se sono stati passati come parametri
026000020204     C                   IF        vlrppt > *blanks
026100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026200020204     C                   EXSR      CHKNUM
026300020204     C                   IF        PiInt=*on
026400020204     C                   Z-ADD     PiVal         VABCCM
026500020204     C                   ENDIF
026600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026700020204     C                   EXSR      CHKNUM
026800020204     C                   IF        PiInt=*on
026900020204     C                   Z-ADD     PiVal         VABLNP
027000020204     C                   ENDIF
027100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027200020204     C                   EXSR      CHKNUM
027300020204     C                   IF        PiInt=*on
027400020204     C                   Z-ADD     PiVal         VABCTR
027500020204     C                   ENDIF
027600020204     C                   ENDIF
027700020204     C*
027800010330     C                   ENDSR
027900010607     C*----------------------------------------------------*
028000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028100010607     C*----------------------------------------------------*
028200010607     C     IMPVAB        BEGSR
028300010607     C*
028400010607     C                   SETOFF                                       3132
028500030515     C*
028600010607     C                   MOVEL     datcor        VABAAS
028700010607     C                   MOVE      datcor        VABMGS
028800010607     C*
028900010607     C* Reperimento campi ALFA
029000010607     C*
029100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
029200030606     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
029300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029500020117     C* ==
029600030606     C                   EVAL      VABRD2=%trim(%subst(vindta:17+35:40-35))+
029700030606     C                                    ' tel.'+%trim(%subst(vindta:92:15))
029800030606     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
029900030606     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
030000030606     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
030100030606     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
030200030606     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
030300030606     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
030400030606     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
030500030606     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
030600060823     C                   EVAL      wTelDest=%trim(%subst(vindta:92:15))
030700100702     C* "particolarità"
030800090303     C                   SELECT
030900090303     C                   WHEN      %subst(vindta:14:1) = *blanks
031000090303     C                   EVAL      VABCCM = 0931845
031100090303     C                   WHEN      %subst(vindta:14:1) = 'N'
031200090303     C                   EVAL      VABCCM = 0931845
031300090303     C                   WHEN      %subst(vindta:14:1) = 'I'
031400090303     C                   EVAL      VABCCM = 0933300
031500090303     C                   WHEN      %subst(vindta:14:1) = 'Y'
031600090303     C                   EVAL      VABCCM = 0933511
031700090303     C                   WHEN      %subst(vindta:14:1) = 'Q'
031800090303     C                   EVAL      VABCCM = 0933461
031900090303     C                   ENDSL
032000100702     C*
032100100702     C                   SELECT
032200100702     C                   WHEN      %subst(vindta:12:1) = 'B'
032300100702     C                   EVAL      VABTC1='A'
032400100702     C                   WHEN      %subst(vindta:12:1) = 'F'
032500100702     C                   EVAL      VABTC1='P'
032600100702     C                   WHEN      %subst(vindta:12:1) = 'C'
032700100702     C                   EVAL      VABTSP='E'
032800100702     C                   WHEN      %subst(vindta:12:1) = 'E'
032900100702     C                   EVAL      VABCCM = 0933510
033000100702     C                   EVAL      VABTC1='P'
033100100702     C                   EVAL      VABTC2='A'
033200100702     C                   ENDSL
033300010607     C*
033400010607     C* Reperimento campi NUMERICI
033500020604     C*
033600030515     C* CAD
033700030606     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033800010607     C                   EXSR      CHKNUM
033900010607     C                   IF        PiInt=*on
034000020204     C                   Z-ADD     PiVal         Num5_0
034100020204     C                   MOVEL(p)  Num5_0        VABCAD
034200010607     C                   ELSE
034300010607     C                   SETON                                        32
034400020204     C                   EVAL      VABCAD = *zeros
034500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034600020204     C                             + ' ' + 'VABCAD'
034700010607     C                   ENDIF
034800030515     C* RMN/NSP
034900030606     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
035000011023     C                   EXSR      CHKNUM
035100011023     C                   IF        PiInt=*on
035200030225     C                   Z-ADD     PiVal         VABRMN
035300030225     C                   Z-ADD     PiVal         VABNSP
035400011023     C                   ELSE
035500020205     C                   EVAL      VABRMN = 1
035600011023     C                   ENDIF
035700030515     C* NCL
035800030606     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035900010607     C                   EXSR      CHKNUM
036000010607     C                   IF        PiInt=*on
036100010607     C                   Z-ADD     PiVal         VABNCL
036200010607     C                   ELSE
036300010607     C                   SETON                                        32
036400010607     C                   Z-ADD     *zeros        VABNCL
036500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036600010607     C                             + ' ' + 'VABNCL'
036700010607     C                   ENDIF
036800030515     C* PKB
036900030606     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
037000030606     C                   EVAL      PiDecChr = '.'
037100010607     C                   EXSR      CHKNUM
037200010607     C                   IF        PiNum=*on
037300010607     C                   Z-ADD(H)  PiVal         VABPKB
037400010607     C                   ELSE
037500010607     C                   SETON                                        32
037600010607     C                   Z-ADD     *zeros        VABPKB
037700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037800010607     C                             + ' ' + 'VABPKB'
037900010607     C                   ENDIF
038000030606     C* CAS
038100030606     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
038200030606     C                   IF        PiStr <> *blanks AND
038300030606     C                             PiStr <> *zeros
038400030606     C                   MOVEL     '1'           FlgCAS
038500030606     C                   EVAL      PiDecChr = ','
038600030606     C                   EXSR      CHKNUM
038700030606     C                   IF        PiNum=*on
038800030606     C                   Z-ADD     PiVal         VABCAS
038900030606     C                   ELSE
039000030606     C                   SETON                                        32
039100030606     C                   Z-ADD     *zeros        VABCAS
039200030606     C                   EVAL      vinmsg = %trimr(vinmsg)
039300030606     C                             + ' ' + 'VABCAS'
039400030606     C                   ENDIF
039500030606     C                   ENDIF
039600030606     C* IAS
039700030606     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
039800030606     C                   EVAL      PiDecChr = ','
039900030606     C                   EXSR      CHKNUM
040000030606     C                   IF        PiNum=*on
040100030606     C                   Z-ADD     PiVal         VABIAS
040200030606     C                   ELSE
040300030606     C                   SETON                                        32
040400030606     C                   Z-ADD     *zeros        VABIAS
040500030606     C                   EVAL      vinmsg = %trimr(vinmsg)
040600030606     C                             + ' ' + 'VABIAS'
040700030606     C                   ENDIF
040800030606     C*
040900030606     C* Considerazioni finali su CBO/CAS
041000030606     C                   IF        FlgCAS = '1'
041100030606     C                   EVAL      VABCBO = '4'
041200030606     C                   ELSE
041300030606     C                   EVAL      VABCBO = '1'
041400030606     C                   ENDIF
041500020204     C*
041600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041700020204     C                   EXSR      CHKIMPDIV
041800020204     C*
041900010607     C                   ENDSR
042000010607     C*----------------------------------------------------*
042100040628
042200040628
042300040628
042400040628     C*----------------------------------------------------*
042500040628     C*  WRIVAT - SCRITTURA RECORD DETTAGLIO COLLI DISK "C" - FILE FIVAT
042600040628     C*----------------------------------------------------*
042700040628     C     WRIVAT        BEGSR
042800040628     C*
042900040628     C                   EVAL      VATFGS = VABFGS
043000040628     C                   EVAL      VATCCM = VABCCM
043100040628     C                   EVAL      VATLNP = VABLNP
043200040628     C                   EVAL      VATAAS = VABAAS
043300040628     C                   EVAL      VATNSP = VABNSP
043400040628     C                   EVAL      VATTRC = 'E'
043500040628     C*
043600040628     C* X ogni collo della testata bolle scrivo 1 record d VAT
043700040628     C                   Z-ADD     1             wNCL              5 0
043800040628     C                   MOVEL     *blanks       wNCLALfa          4
043900040628     C                   DOW       wNCL <= VABNCL
044000040628     C                   MOVE(P)   wNCL          wNCLALfa
044100130521     C                   EVAL      wChiSono = %subst(vindta:331:14) +
044200130521     C                                        wNCLAlfa
044300130521     C                   MOVEL     *zeros        wVATNOT          18
044400060113     C                   MOVE      wChiSono      wVATNOT
044500060113     C                   EVAL      VATNOT = wVATNOT
044600040628     C                   WRITE(e)  FIVAT000
044700040628     C                   IF        not %error
044800040628     C                   ADD       1             §CTROKVT          7 0
044900040628     C                   ENDIF
045000040628     C                   ADD       1             wNCL
045100040628     C                   ENDDO
045200060823     C*
045300060823     C* Scrivo inoltre 1 record x il telefono destinatario (tipo record 'B')
045400060823     C                   EVAL      VATNOT = wTelDest
045500060823     C                   EVAL      VATTRC = 'B'
045600060823     C                   WRITE(e)  FIVAT000
045700060823     C                   IF        not %error
045800060823     C                   ADD       1             §CTROKVT          7 0
045900060823     C                   ENDIF
046000040628     C*
046100040628     C                   ENDSR
046200040628     C*----------------------------------------------------*
046300040628
046400040628
046500040628
046600040628     C*----------------------------------------------------*
046700040628     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046800040628     C*----------------------------------------------------*
046900040628     C     PREVAT        BEGSR
047000040628     C*
047100040628     C* Compongo il nome del membro da dare al FIVATWWR
047200040628     C                   eval      parmbr = vlrhdl
047300040628     C                   movel     'M'           parmbr
047400040628     C                   eval      parccm = '00931845'
047500040628     C                   eval      paropz = '1'
047600040628     C* Effettuo la chiamata al CLLE preposto
047700040628     C                   call(e)   'TITVVTC'
047800040628     C                   parm                    parccm
047900040628     C                   parm                    parmbr
048000040628     C                   parm                    paropz
048100040628     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048200040628     C                   if        %error
048300040628     C                   movel     '1'           chkcall
048400040628     C                   else
048500040628     C                   movel     '0'           chkcall
048600040628     C                   endif
048700040628     C*
048800040628     C                   ENDSR
048900010601
049000020204
049100020204     C*----------------------------------------------------*
049200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049300020204     C*----------------------------------------------------*
049400020204     C     CHKIMPDIV     BEGSR
049500020204     C*
049600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049700020204     C                   Z-ADD     *zeros        wrkDec            9 9
049800020204     C*
049900020204     C* Come prima cosa effettuo considerazioni sulla divisa
050000020204     C                   IF        vabIAS > *zeros
050100020204     C                   IF        vabVAS <> 'EUR'
050200020204     C                   EVAL      vabVAS =  'ITL'
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C                   IF        vabCAS > *zeros
050700020204     C                   IF        vabVCA <> 'EUR'
050800020204     C                   EVAL      vabVCA =  'ITL'
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C                   IF        vabVMD > *zeros
051300020204     C                   IF        vabVAD <> 'EUR'
051400020204     C                   EVAL      vabVAD =  'ITL'
051500020204     C                   ENDIF
051600020204     C                   ENDIF
051700020204     C*
051800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051900020204     C                   Z-ADD     vabIAS        wrkDec
052000020204     C                   IF        wrkDec > *zeros
052100020204     C                   IF        vabVAS = 'ITL'
052200020204     C                   EVAL      vabIAS = *zeros
052300020204     C                   ENDIF
052400020204     C                   ENDIF
052500020204     C*
052600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052700020204     C                   Z-ADD     vabCAS        wrkDec
052800020204     C                   IF        wrkDec > *zeros
052900020204     C                   IF        vabVCA = 'ITL'
053000020204     C                   EVAL      vabCAS = *zeros
053100020204     C                   ENDIF
053200020204     C                   ENDIF
053300020204     C*
053400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053500020204     C                   Z-ADD     vabVMD        wrkDec
053600020204     C                   IF        wrkDec > *zeros
053700020204     C                   IF        vabVAD = 'ITL'
053800020204     C                   EVAL      vabVMD = *zeros
053900020204     C                   ENDIF
054000020204     C                   ENDIF
054100020204     C*
054200020204     C                   ENDSR
054300020204     C***
054400020204
054500010330
054600010330
054700010330     C*----------------------------------------------------*
054800010330     C*  CONTROLLO NUMERICITA' CAMPI
054900010330     C*----------------------------------------------------*
055000010330     C     CHKNUM        BEGSR
055100010330     C*
055200010606     C                   IF        PiDecChr = *blanks
055300030606     C                   EVAL      PiDecChr = ','
055400010606     C                   ENDIF
055500010606     C*
055600010606     C                   CALL(e)   'ISNUMERIC'
055700010330     C                   PARM                    PiStr            30
055800010606     C                   PARM                    PiDecChr          1
055900010330     C                   PARM      *ZEROS        PiVal            30 9
056000010330     C                   PARM      '0'           PiInt             1
056100010330     C                   PARM      '0'           PiNum             1
056200010330     C                   IF        %error
056300010606     C                   EVAL      PiNum=*off
056400010330     C                   ENDIF
056500010330     C*
056600010330     C                   ENDSR
056700010330     C***
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
059700040628
059800040628
059900040628
060000040628      /TITLE Invio dei dati al punto operativo.
060100040628     C     invio         BEGSR
060200040628     C*
060300040628     C* 1° invio FIVAT
060400040628     C                   reset                   dscmz
060500040628     C                   move      vlrpoi        cmzdst
060600040628     C                   eval      cmzfld = 'FIVATWWR'
060700040628     C                   eval      cmzmbd = vlrhdl
060800040628     C                   eval      %subst(cmzmbd:1:1) = 'M'
060900040628     C***                if        prmfir = *blanks
061000040628     C                   eval      cmzfla = 'FIVAT00F'
061100040628     C                   eval      cmzmba = 'FIVAT00F'
061200040628     C***                else
061300040628     C***                eval      cmzfla = prmfir
061400040628     C***                eval      cmzmba = prmfir
061500040628     C***                endif
061600040628     C                   eval      cmznrr = *zeros
061700040628     C                   move      §ctrokvt      cmznrr
061800040628     C                   eval      cmzlba = vlrfl1
061900040628     C                   call(e)   'TIS711C'
062000040628     C                   parm                    dscmz
062100040628     C                   parm      *blanks       esito
062200040628     C                   if        %error
062300040628     C                             or cmzerr = '1'
062400040628     C                             or esito  = '1'
062500040628     C                   eval      wrkesito = '3'
062600040628     C                   else
062700040628     C*
062800040628     C* 2° invio FIVAB
062900040628     C                   reset                   dscmz
063000040628     C                   move      vlrpoi        cmzdst
063100040628     C                   eval      cmzfld = vlrfou
063200040628     C                   eval      cmzmbd = vlrhdl
063300040628     C                   eval      %subst(cmzmbd:1:1) = 'M'
063400040628     C***                if        prmfir = *blanks
063500040628     C                   eval      cmzfla = 'FIVAB00F'
063600040628     C                   eval      cmzmba = 'FIVAB00F'
063700040628     C***                else
063800040628     C***                eval      cmzfla = prmfir
063900040628     C***                eval      cmzmba = prmfir
064000040628     C***                endif
064100040628     C                   eval      cmznrr = *zeros
064200040628     C                   move      §ctrokvb      cmznrr
064300040628     C                   eval      cmzlba = vlrfl1
064400040628     C                   call(e)   'TIS711C'
064500040628     C                   parm                    dscmz
064600040628     C                   parm      *blanks       esito
064700040628     C                   if        %error
064800040628     C                             or cmzerr = '1'
064900040628     C                             or esito  = '1'
065000040628     C                   eval      wrkesito = '3'
065100040628     C                   endif
065200040628     C                   endif
065300040628     C*
065400040628     C                   ENDSR
065500040628     C***
065600010601
065700010601
065800010601
065900010601      /TITLE Invio dei dati al punto operativo.
066000010601     C     opeini        BEGSR
066100010601     C*
066200010601     C* Inizializzo flag e contatori operativi
066300010601     C                   movel     '0'           flgGiro           1
066400010601     C                   movel     '0'           flgMulti          1
066500010601     C                   movel     '1'           flgStato          1
066600010615     C                   movel     '0'           flgOk             1
066700010601     C                   z-add     *zeros        cntNonEl         10 0
066800010601     C                   z-add     *zeros        depfil            3 0
066900010601     C                   z-add     *zeros        invfil            3 0
067000010601     C*
067100010601     C                   ENDSR
067200010601     C***
067300010601
067400010601
067500010601
067600010330
067700010330
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500010330     C*
068600010330     C* CALCOLA LA DATA CORRENTE
068700010330     C                   time                    wn14             14 0
068800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068900010330     C                   z-add     wn8           g08dat
069000010330     C                   z-add     *zeros        g08inv
069100010330     C                   movel     '0'           g08err
069200010330     C                   call      'XSRDA8'
069300010330     C                   parm                    wlbda8
069400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069500000613     C*
069600000613     C                   ENDSR
069700000613     C***
