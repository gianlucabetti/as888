000100020916      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200130116     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVADwwr  O    E             DISK    usropn
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
002900051212
003000051212     D TxtInOut        S           2048
003100051212     D ElencoChar      S            256
003200051212     D TipoElenco      S              1
003300051212     D CharSost        S              1
003400051212     D UpperCase       S              1
003500051212     D ChkNull         S              1
003600051212     D CharNull        S              1
003700051212     D EsitoBon        S              1
003800010330
003900010330
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300010601     C*
004400010601     C                   exsr      opeini
004500010601     C                   exsr      rwvad
004600010601     C*
004700010601     C                   seton                                        lr
004800010601
004900010601
005000010601
005100010601
005200010601     C*--------------------------------------------------------
005300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005400010601     C*--------------------------------------------------------
005500010601     C     PREELA        BEGSR
005600010601     C*
005700010601     C* SE OCCORRE SPEDIRE IN FILIALE
005800010601     C                   if        invfil <> *zeros and
005900010601     C                             flgGiro = '0'
006000010601     C*
006100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006200010601     C                   eval      flgGiro = '1'
006300010601     C*
006400010601     C                   endif
006500010601     C*
006600010601     C                   ENDSR
006700010601     C***
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601
007400010601
007500010601     C*--------------------------------------------------------
007600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007700010601     C*--------------------------------------------------------
007800010601     C     ENDELA        BEGSR
007900000616     C*
008000010601     C                   ENDSR
008100010601     C***
008200000613
008300010330
008400010601
008500010601
008600010601
008700010330     C*--------------------------------------------------------
008800020916     C* RWVAD   LEGGE tivin00r E SCRIVE FIVADWWF              *
008900010330     C*--------------------------------------------------------
009000010330     C     RWVAD         BEGSR
009100010330     C*
009200010330     C                   if        not %open(tivin00r)
009300010330     C                   open      tivin00r
009400010330     C                   endif
009500020916     C                   if        not %open(fivadwwr)
009600020916     C                   open      fivadwwr
009700010330     C                   endif
009800010330     C*
009900010604     C                   clear                   §CTROK
010000010604     C                   clear                   §CTRMO
010100010604     C                   clear                   §CTRNO
010200010330     C*
010300010330     C                   DO        *HIVAL
010400010330     C*
010500010330     C                   READ      tivin00r                               70
010600010618     C*
010700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010800010618     C                   if        vinflg = '1'
010900010618     C                   eval      flgOk = '1'
011000010618     C                   endif
011100010618     C*
011200010330     C                   if        vindta > *blanks
011300010330     C                   add       1             rrnum
011400010330     C*
011500010601     C                   if        *in70 = *off and
011600010330     C                             (vinflg = *blanks
011700010330     C                              or vinflg = '0'
011800010330     C                              or vinflg = '2')
011900010330     C*
012000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012100010711     C                   if        vinflg = *blanks or vinflg = '0'
012200010711     C                   clear                   vinmsg
012300010711     C                   endif
012400010601     C*
012500010330     C                   exsr      inzvar
012600010330     C                   exsr      defcam
012700010330     C                   exsr      impvad
012800010601     C*
012900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013000010601     C                   eval      depfil = VADLNP
013100010601     C                   exsr      repfil
013200010601     C                   if        depfil = invfil
013300021014     C                   if        vlrpoi = 999
013400021010     C                   MOVE(P)   invfil        VADFGS
013500021014     C                   else
013600021014     C                   MOVE(P)   vlrpoi        VADFGS
013700021014     C                   endif
013800010601     C*
013900010601     C                   exsr      PREELA
014000051212
014100051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
014200051212     c                   exsr      exeBon
014300051212
014400010601     C*
014500010604     C* Ebbene...
014600010604     C*
014700010604     C  N31              ADD       1             §CTROK            7 0
014800010604     C   32              ADD       1             §CTRMO            7 0
014900010604     C   31              ADD       1             §CTRNO            7 0
015000020916     C  N31              WRITE     FIVAD000
015100010604     C*
015200010604     C                   if        *in31 = *off and
015300010604     C                             *in32 = *off
015400010604     C                   eval      vinflg = '1'
015500010604     C                   else
015600010604     C                   eval      vinflg = '2'
015700010604     C                   endif
015800010604     C                   endif
015900010601     C*
016000010604     C                   endif
016100010604     C*
016200010330     C                   else
016300010330     C                   eval      vinflg = '1'
016400010330     C                   endif
016500010601     C*
016600010601     C  N70              update    tivin000
016700010330     C*
016800010330     C  N70              ENDdo
016900010601     C
017000010601     C*
017100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017200010601     C                   if        cntNonEl = *zeros or
017300010601     C                             flgMulti = '0'
017400010330     C* Se non ci sono record con errori ...
017500010601     C                   if        §ctrno = 0 and
017600010604     C                             §ctrmo = 0 and
017700010601     C                             flgStato <> '2'
017800010330     C* ... restituisco esito OK.
017900010330     C                   eval      wrkesito = '0'
018000010330     C                   else
018100010330     C                   if        §ctrok > 0
018200010330     C                   eval      wrkesito = '1'
018300010330     C                   else
018400010615     C                   if        flgOk = '0'
018500010615     C                   eval      wrkesito = '2'
018600010615     C                   else
018700010615     C                   eval      wrkesito = '6'
018800010615     C                   endif
018900010330     C                   endif
019000010330     C                   endif
019100010601     C                   else
019200010601     C                   eval      wrkesito = '9'
019300010601     C                   endif
019400010330     C*
019500010330     C                   if        %open(tivin00r)
019600010330     C                   close     tivin00r
019700010330     C                   endif
019800020916     C                   if        %open(fivadwwr)
019900020916     C                   close     fivadwwr
020000010330     C                   endif
020100010601     C*
020200010601     C                   if        vlrpoi <> 999
020300010601     C                   eval      invfil = vlrpoi
020400010601     C                   endif
020500010330     C*
020600010330     C                   if        §ctrok > 0
020700010601     C                             and invfil > *zeros
020800010330     C                   exsr      invio
020900010330     C                   endif
021000010612     C*
021100010612     C                   if        flgGiro = '1'
021200010612     C                   exsr      endela
021300010612     C                   endif
021400010330     C*
021500010330     C                   ENDSR
021600010330     C***
021700010330
021800010601
021900010601
022000010601
022100010601
022200010330     C*----------------------------------------------------*
022300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022400010330     C*----------------------------------------------------*
022500010330     C     INZVAR        BEGSR
022600010330     C*
022700010330     C*
022800010330     C                   ENDSR
022900010330     C*----------------------------------------------------*
023000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023100010330     C*----------------------------------------------------*
023200010330     C     DEFCAM        BEGSR
023300010330     C*
023400020916     C                   CLEAR                   FIVAD000
023500010330     C*
023600010330     C                   ENDSR
023700010330     C*----------------------------------------------------*
023800020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAD)
023900010330     C*----------------------------------------------------*
024000010330     C     IMPVAD        BEGSR
024100010330     C*
024200010604     C                   SETOFF                                       3132
024300010330     C*
024400010330     C* Reperimento campi ALFA
024500010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
024600010330     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
024700010330     C*
024800010330     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
024900010330     C                   EXSR      CHKNUM
025000010330     C                   IF        PiInt=*on
025100010330     C                   Z-ADD     PiVal         VADCCM
025200010330     C                   ELSE
025300010604     C                   SETON                                        31
025400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
025500010330     C                             + ' ' + 'VADCCM'
025600010330     C                   ENDIF
025700010330     C*
025800010330     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
025900010330     C                   EXSR      CHKNUM
026000010330     C                   IF        PiInt=*on
026100010330     C                   Z-ADD     PiVal         VADLNP
026200010330     C                   ELSE
026300010604     C                   SETON                                        31
026400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
026500021010     C                             + ' ' + 'VADLNP'
026600010330     C                   ENDIF
026700050104     C*
026800050104     C                   IF        %subst(vindta:12:4) <> *blanks
026900050104     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
027000050104     C                   EXSR      CHKNUM
027100050104     C                   IF        PiInt=*on
027200050104     C                   Z-ADD     PiVal         VADAAS
027300050104     C                   ELSE
027400050104     C                   SETON                                        31
027500050104     C                   EVAL      vinmsg = %trimr(vinmsg)
027600050104     C                             + ' ' + 'VADAAS'
027700050104     C                   ENDIF
027800050104     C                   ELSE
027900050104     C                   MOVEL     datcor        VADAAS
028000050104     C                   ENDIF
028100010330     C*
028200010330     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
028300010330     C                   EXSR      CHKNUM
028400010330     C                   IF        PiInt=*on
028500010330     C                   Z-ADD     PiVal         VADNRS
028600010330     C                   ELSE
028700010604     C                   SETON                                        32
028800010604     C                   Z-ADD     *zeros        VADNRS
028900010330     C                   EVAL      vinmsg = %trimr(vinmsg)
029000010330     C                             + ' ' + 'VADNRS'
029100010330     C                   ENDIF
029200010330     C*
029300010330     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
029400010330     C                   EXSR      CHKNUM
029500010330     C                   IF        PiInt=*on
029600010330     C                   Z-ADD     PiVal         VADNSP
029700010330     C                   ELSE
029800010604     C                   SETON                                        32
029900010604     C                   Z-ADD     *zeros        VADNSP
030000010330     C                   EVAL      vinmsg = %trimr(vinmsg)
030100010330     C                             + ' ' + 'VADNSP'
030200010330     C                   ENDIF
030300010330     C*
030400010330     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
030500010330     C                   EXSR      CHKNUM
030600010330     C                   IF        PiInt=*on
030700010330     C                   Z-ADD     PiVal         VADNCL
030800010330     C                   ELSE
030900010604     C                   SETON                                        32
031000010604     C                   Z-ADD     *zeros        VADNCL
031100010330     C                   EVAL      vinmsg = %trimr(vinmsg)
031200010330     C                             + ' ' + 'VADNCL'
031300010330     C                   ENDIF
031400010330     C*
031500010330     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
031600010330     C                   EXSR      CHKNUM
031700010330     C                   IF        PiInt=*on
031800010330     C                   Z-ADD     PiVal         VADNCD
031900010330     C                   ELSE
032000010604     C                   SETON                                        32
032100010604     C                   Z-ADD     *zeros        VADNCD
032200010330     C                   EVAL      vinmsg = %trimr(vinmsg)
032300010330     C                             + ' ' + 'VADNCD'
032400010330     C                   ENDIF
032500010330     C*
032600010330     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
032700010330     C                   EXSR      CHKNUM
032800010330     C                   IF        PiInt=*on
032900010330     C                   Z-ADD     PiVal         VADNCA
033000010330     C                   ELSE
033100010604     C                   SETON                                        32
033200010604     C                   Z-ADD     *zeros        VADNCA
033300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
033400010330     C                             + ' ' + 'VADNCA'
033500010330     C                   ENDIF
033600010330     C*
033700010330     C                   ENDSR
033800990920
033900010601
034000010601
034100010330
034200010330
034300010330     C*----------------------------------------------------*
034400010330     C*  CONTROLLO NUMERICITA' CAMPI
034500010330     C*----------------------------------------------------*
034600010330     C     CHKNUM        BEGSR
034700010330     C*
034800010330     C                   call(e)   'ISNUMERIC'
034900010330     C                   PARM                    PiStr            30
035000010330     C                   PARM      '.'           PiDecChr          1
035100010330     C                   PARM      *ZEROS        PiVal            30 9
035200010330     C                   PARM      '0'           PiInt             1
035300010330     C                   PARM      '0'           PiNum             1
035400010330     C                   IF        %error
035500010330     C                   EVAL      PiInt=*off
035600010330     C                   ENDIF
035700010330     C*
035800010330     C                   ENDSR
035900010330     C***
036000010330
036100010601
036200010601
036300010601
036400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036500010601     C     repfil        BEGSR
036600010601     C*
036700010601     C                   if        invfil = *zeros and
036800010601     C                             depfil > *zeros and
036900010613     C                             (vinflg = *blanks or
037000010613     C                              vinflg = *zeros)
037100010601     C
037200010601     C                   eval      invfil = depfil
037300010601     C                   endif
037400010601     C*
037500010601     C                   if        depfil <> invfil and
037600010601     C                             invfil > *zeros
037700010601     C                   eval      flgMulti = '1'
037800010601     C                   if        vinflg = *blanks
037900010601     C                   add       1             cntNonEl
038000010601     C                   endif
038100010601     C                   endif
038200010601     C*
038300010601     C                   if        vinflg = '2'
038400010601     C                   eval      flgStato = '2'
038500010601     C                   endif
038600010601     C*
038700010601     C                   ENDSR
038800010601     C***
038900010601
039000010601
039100010601
039200010330
039300010330
039400010330
039500990920      /TITLE Invio dei dati al punto operativo.
039600000613     C     invio         BEGSR
039700990920     C*
039800990920     C                   reset                   dscmz
039900010601     C                   move      invfil        cmzdst
040000990920     C                   eval      cmzfld = vlrfou
040100990920     C                   eval      cmzmbd = vlrhdl
040200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
040300000710     C                   if        prmfir = *blanks
040400020916     C                   eval      cmzfla = 'FIVAD00F'
040500020916     C                   eval      cmzmba = 'FIVAD00F'
040600000710     C                   else
040700000710     C                   eval      cmzfla = prmfir
040800000710     C                   eval      cmzmba = prmfir
040900000710     C                   endif
041000990920     C                   eval      cmznrr = *zeros
041100990920     C                   move      §ctrok        cmznrr
041200021003     C                   eval      cmzlba = vlrfl1
041300990920     C                   call(e)   'TIS711C'
041400990920     C                   parm                    dscmz
041500990921     C                   parm      *blanks       esito
041600990920     C                   if        %error
041700990920     C                             or cmzerr = '1'
041800990921     C                             or esito  = '1'
041900000710     C                   eval      wrkesito = '3'
042000990920     C                   endif
042100990920     C*
042200000613     C                   ENDSR
042300990910
042400010601
042500010601
042600010601
042700010601
042800010601      /TITLE Invio dei dati al punto operativo.
042900010601     C     opeini        BEGSR
043000010601     C*
043100010601     C* Inizializzo flag e contatori operativi
043200010601     C                   movel     '0'           flgGiro           1
043300010601     C                   movel     '0'           flgMulti          1
043400010601     C                   movel     '1'           flgStato          1
043500010615     C                   movel     '0'           flgOk             1
043600010601     C                   z-add     *zeros        cntNonEl         10 0
043700010601     C                   z-add     *zeros        depfil            3 0
043800010601     C                   z-add     *zeros        invfil            3 0
043900030407     C*
044000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
044100030407     C                   if        %subst(vlrppt:1:1) = 'S'
044200030407     C                   seton                                        50
044300030407     C                   else
044400030407     C                   setoff                                       50
044500030407     C                   endif
044600010601     C*
044700010601     C                   ENDSR
044800010601     C***
044900010601
045000051212
045100051212
045200051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
045300051212     C     exeBon        BEGSR
045400051212     C*
045500051212     C* Imposto i parametri x il *pgm d "bonifica"
045600051212     C                   EVAL      ElencoChar = *blanks
045700051212     C                   EVAL      TipoElenco = *blanks
045800051212     C                   EVAL      CharSost   = *blanks
045900051212     C                   EVAL      UpperCase  = '1'
046000051212     C                   EVAL      ChkNull    = '1'
046100051212     C                   EVAL      CharNull   = *blanks
046200051212     C*
046300051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
046400051212     C                   EVAL      TxtInOut   = VADCDP
046500051212     C                   EVAL      EsitoBon   = *blanks
046600051212     C*
046700051212     C                   CALL      'XCHKCHAR'
046800051212     C                   PARM                    TxtInOut
046900051212     C                   PARM                    ElencoChar
047000051212     C                   PARM                    TipoElenco
047100051212     C                   PARM                    CharSost
047200051212     C                   PARM                    UpperCase
047300051212     C                   PARM                    ChkNull
047400051212     C                   PARM                    CharNull
047500051212     C                   PARM                    EsitoBon
047600051212     C*
047700051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
047800051212     C                   IF        EsitoBon = '1'
047900051212     C                   EVAL      VADCDP = TxtInOut
048000051212     C                   ENDIF
048100051212     C*
048200051212     C                   ENDSR
048300051212
048400010601
048500010601
048600010330
048700010330
048800000613     C     *inzsr        BEGSR
048900990910     C*
049000990910     C     *entry        plist
049100990920     C                   parm                    tivlrds
049200990921     C                   parm      wrkesito      esito
049300000724     C                   parm                    prmlit
049400000710     C                   parm                    prmfir
049500010330     C*
049600010330     C* CALCOLA LA DATA CORRENTE
049700010330     C                   time                    wn14             14 0
049800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
049900010330     C                   z-add     wn8           g08dat
050000010330     C                   z-add     *zeros        g08inv
050100010330     C                   movel     '0'           g08err
050200010330     C                   call      'XSRDA8'
050300010330     C                   parm                    wlbda8
050400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
050500000613     C*
050600000613     C                   ENDSR
050700000613     C***
