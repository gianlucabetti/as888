000100121107      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500121107     FFIVADwwr  O    E             DISK    usropn
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
001900000313     D*
002000051212
002100051212     D TxtInOut        S           2048
002200051212     D ElencoChar      S            256
002300051212     D TipoElenco      S              1
002400051212     D CharSost        S              1
002500051212     D UpperCase       S              1
002600051212     D ChkNull         S              1
002700051212     D CharNull        S              1
002800051212     D EsitoBon        S              1
002900010330
003000010330
003100010330
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010601     C*
003600010601     C                   exsr      opeini
003700010601     C                   exsr      rwvad
003800010601     C*
003900010601     C                   seton                                        lr
004000010601
004100010601
004200010601
004300010601
004400010601     C*--------------------------------------------------------
004500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004600010601     C*--------------------------------------------------------
004700010601     C     PREELA        BEGSR
004800010601     C*
004900010601     C* SE OCCORRE SPEDIRE IN FILIALE
005000010601     C                   if        invfil <> *zeros and
005100010601     C                             flgGiro = '0'
005200010601     C*
005300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005400010601     C                   eval      flgGiro = '1'
005500010601     C*
005600010601     C                   endif
005700010601     C*
005800010601     C                   ENDSR
005900010601     C***
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006300010601     C*--------------------------------------------------------
006400010601     C     ENDELA        BEGSR
006500000616     C*
006600010601     C                   ENDSR
006700010601     C***
006800010601
006900010330     C*--------------------------------------------------------
007000121107     C* RWVAD   LEGGE tivin00r E SCRIVE FIVADWWF              *
007100010330     C*--------------------------------------------------------
007200010330     C     RWVAD         BEGSR
007300010330     C*
007400010330     C                   if        not %open(tivin00r)
007500010330     C                   open      tivin00r
007600010330     C                   endif
007700121107     C                   if        not %open(fivadwwr)
007800121107     C                   open      fivadwwr
007900010330     C                   endif
008000010330     C*
008100010604     C                   clear                   §CTROK
008200010604     C                   clear                   §CTRMO
008300010604     C                   clear                   §CTRNO
008400010330     C*
008500010330     C                   DO        *HIVAL
008600010330     C*
008700010330     C                   READ      tivin00r                               70
008800010618     C*
008900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009000010618     C                   if        vinflg = '1'
009100010618     C                   eval      flgOk = '1'
009200010618     C                   endif
009300010618     C*
009400010330     C                   if        vindta > *blanks
009500010330     C                   add       1             rrnum
009600010330     C*
009700010601     C                   if        *in70 = *off and
009800010330     C                             (vinflg = *blanks
009900010330     C                              or vinflg = '0'
010000010330     C                              or vinflg = '2')
010100010330     C*
010200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010300010711     C                   if        vinflg = *blanks or vinflg = '0'
010400010711     C                   clear                   vinmsg
010500010711     C                   endif
010600010601     C*
010700010330     C                   exsr      inzvar
010800010330     C                   exsr      defcam
010900010330     C                   exsr      impvad
011000010601     C*
011100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011200010601     C                   eval      depfil = VADLNP
011300010601     C                   exsr      repfil
011400010601     C                   if        depfil = invfil
011500021014     C                   if        vlrpoi = 999
011600021010     C                   MOVE(P)   invfil        VADFGS
011700021014     C                   else
011800021014     C                   MOVE(P)   vlrpoi        VADFGS
011900021014     C                   endif
012000010601     C*
012100010601     C                   exsr      PREELA
012200051212
012300051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
012400051212     c                   exsr      exeBon
012500051212
012600010601     C*
012700010604     C* Ebbene...
012800010604     C*
012900010604     C  N31              ADD       1             §CTROK            7 0
013000010604     C   32              ADD       1             §CTRMO            7 0
013100010604     C   31              ADD       1             §CTRNO            7 0
013200121107     C  N31              WRITE     FIVAD000
013300010604     C*
013400010604     C                   if        *in31 = *off and
013500010604     C                             *in32 = *off
013600010604     C                   eval      vinflg = '1'
013700010604     C                   else
013800010604     C                   eval      vinflg = '2'
013900010604     C                   endif
014000010604     C                   endif
014100010601     C*
014200010604     C                   endif
014300010604     C*
014400010330     C                   else
014500010330     C                   eval      vinflg = '1'
014600010330     C                   endif
014700010601     C*
014800010601     C  N70              update    tivin000
014900010330     C*
015000010330     C  N70              ENDdo
015100010601     C
015200010601     C*
015300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015400010601     C                   if        cntNonEl = *zeros or
015500010601     C                             flgMulti = '0'
015600010330     C* Se non ci sono record con errori ...
015700010601     C                   if        §ctrno = 0 and
015800010604     C                             §ctrmo = 0 and
015900010601     C                             flgStato <> '2'
016000010330     C* ... restituisco esito OK.
016100010330     C                   eval      wrkesito = '0'
016200010330     C                   else
016300010330     C                   if        §ctrok > 0
016400010330     C                   eval      wrkesito = '1'
016500010330     C                   else
016600010615     C                   if        flgOk = '0'
016700010615     C                   eval      wrkesito = '2'
016800010615     C                   else
016900010615     C                   eval      wrkesito = '6'
017000010615     C                   endif
017100010330     C                   endif
017200010330     C                   endif
017300050613     C*
017400050613     C                   else
017500050613     C                   eval      wrkesito = '8'
017600050613     C                   eval      flgGiro = '1'
017700050613     C                   endif
017800010330     C*
017900010330     C                   if        %open(tivin00r)
018000010330     C                   close     tivin00r
018100010330     C                   endif
018200121107     C                   if        %open(fivadwwr)
018300121107     C                   close     fivadwwr
018400010330     C                   endif
018500010601     C*
018600010601     C                   if        vlrpoi <> 999
018700010601     C                   eval      invfil = vlrpoi
018800010601     C                   endif
018900010330     C*
019000010330     C                   if        §ctrok > 0
019100010601     C                             and invfil > *zeros
019200010330     C                   exsr      invio
019300010330     C                   endif
019400010612     C*
019500010612     C                   if        flgGiro = '1'
019600010612     C                   exsr      endela
019700010612     C                   endif
019800010330     C*
019900010330     C                   ENDSR
020000010330     C***
020100010330
020200010601
020300010601
020400010601
020500010601
020600010330     C*----------------------------------------------------*
020700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020800010330     C*----------------------------------------------------*
020900010330     C     INZVAR        BEGSR
021000010330     C*
021100010330     C*
021200010330     C                   ENDSR
021300010330     C*----------------------------------------------------*
021400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
021500010330     C*----------------------------------------------------*
021600010330     C     DEFCAM        BEGSR
021700010330     C*
021800121107     C                   CLEAR                   FIVAD000
021900010330     C*
022000010330     C                   ENDSR
022100010330     C*----------------------------------------------------*
022200121107     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAD)
022300010330     C*----------------------------------------------------*
022400010330     C     IMPVAD        BEGSR
022500010330     C*
022600010604     C                   SETOFF                                       3132
022700010330     C*
022800010330     C* Reperimento campi ALFA
022900010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
023000010330     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
023100010330     C*
023200010330     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
023300010330     C                   EXSR      CHKNUM
023400010330     C                   IF        PiInt=*on
023500010330     C                   Z-ADD     PiVal         VADCCM
023600010330     C                   ELSE
023700010604     C                   SETON                                        31
023800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
023900010330     C                             + ' ' + 'VADCCM'
024000010330     C                   ENDIF
024100010330     C*
024200010330     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
024300010330     C                   EXSR      CHKNUM
024400010330     C                   IF        PiInt=*on
024500010330     C                   Z-ADD     PiVal         VADLNP
024600010330     C                   ELSE
024700010604     C                   SETON                                        31
024800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
024900021010     C                             + ' ' + 'VADLNP'
025000010330     C                   ENDIF
025100050104     C*
025200050104     C                   IF        %subst(vindta:12:4) <> *blanks
025300050104     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
025400050104     C                   EXSR      CHKNUM
025500050104     C                   IF        PiInt=*on
025600050104     C                   Z-ADD     PiVal         VADAAS
025700050104     C                   ELSE
025800050104     C                   SETON                                        31
025900050104     C                   EVAL      vinmsg = %trimr(vinmsg)
026000050104     C                             + ' ' + 'VADAAS'
026100050104     C                   ENDIF
026200050104     C                   ELSE
026300050104     C                   MOVEL     datcor        VADAAS
026400050104     C                   ENDIF
026500010330     C*
026600010330     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
026700010330     C                   EXSR      CHKNUM
026800010330     C                   IF        PiInt=*on
026900010330     C                   Z-ADD     PiVal         VADNRS
027000010330     C                   ELSE
027100010604     C                   SETON                                        32
027200010604     C                   Z-ADD     *zeros        VADNRS
027300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
027400010330     C                             + ' ' + 'VADNRS'
027500010330     C                   ENDIF
027600010330     C*
027700010330     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
027800010330     C                   EXSR      CHKNUM
027900010330     C                   IF        PiInt=*on
028000010330     C                   Z-ADD     PiVal         VADNSP
028100010330     C                   ELSE
028200010604     C                   SETON                                        32
028300010604     C                   Z-ADD     *zeros        VADNSP
028400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
028500010330     C                             + ' ' + 'VADNSP'
028600010330     C                   ENDIF
028700010330     C*
028800010330     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
028900010330     C                   EXSR      CHKNUM
029000010330     C                   IF        PiInt=*on
029100010330     C                   Z-ADD     PiVal         VADNCL
029200010330     C                   ELSE
029300010604     C                   SETON                                        32
029400010604     C                   Z-ADD     *zeros        VADNCL
029500010330     C                   EVAL      vinmsg = %trimr(vinmsg)
029600010330     C                             + ' ' + 'VADNCL'
029700010330     C                   ENDIF
029800010330     C*
029900010330     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
030000010330     C                   EXSR      CHKNUM
030100010330     C                   IF        PiInt=*on
030200010330     C                   Z-ADD     PiVal         VADNCD
030300010330     C                   ELSE
030400010604     C                   SETON                                        32
030500010604     C                   Z-ADD     *zeros        VADNCD
030600010330     C                   EVAL      vinmsg = %trimr(vinmsg)
030700010330     C                             + ' ' + 'VADNCD'
030800010330     C                   ENDIF
030900010330     C*
031000010330     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
031100010330     C                   EXSR      CHKNUM
031200010330     C                   IF        PiInt=*on
031300010330     C                   Z-ADD     PiVal         VADNCA
031400010330     C                   ELSE
031500010604     C                   SETON                                        32
031600010604     C                   Z-ADD     *zeros        VADNCA
031700010330     C                   EVAL      vinmsg = %trimr(vinmsg)
031800010330     C                             + ' ' + 'VADNCA'
031900010330     C                   ENDIF
032000010330     C*
032100010330     C                   ENDSR
032200990920
032300010601
032400010601
032500010330
032600010330
032700010330     C*----------------------------------------------------*
032800010330     C*  CONTROLLO NUMERICITA' CAMPI
032900010330     C*----------------------------------------------------*
033000010330     C     CHKNUM        BEGSR
033100010330     C*
033200010330     C                   call(e)   'ISNUMERIC'
033300010330     C                   PARM                    PiStr            30
033400010330     C                   PARM      '.'           PiDecChr          1
033500010330     C                   PARM      *ZEROS        PiVal            30 9
033600010330     C                   PARM      '0'           PiInt             1
033700010330     C                   PARM      '0'           PiNum             1
033800010330     C                   IF        %error
033900010330     C                   EVAL      PiInt=*off
034000010330     C                   ENDIF
034100010330     C*
034200010330     C                   ENDSR
034300010330     C***
034400010330
034500010601
034600010601
034700010601
034800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
034900010601     C     repfil        BEGSR
035000010601     C*
035100010601     C                   if        invfil = *zeros and
035200010601     C                             depfil > *zeros and
035300010613     C                             (vinflg = *blanks or
035400010613     C                              vinflg = *zeros)
035500010601     C
035600010601     C                   eval      invfil = depfil
035700010601     C                   endif
035800010601     C*
035900010601     C                   if        depfil <> invfil and
036000010601     C                             invfil > *zeros
036100010601     C                   eval      flgMulti = '1'
036200010601     C                   if        vinflg = *blanks
036300010601     C                   add       1             cntNonEl
036400010601     C                   endif
036500010601     C                   endif
036600010601     C*
036700010601     C                   if        vinflg = '2'
036800010601     C                   eval      flgStato = '2'
036900010601     C                   endif
037000010601     C*
037100010601     C                   ENDSR
037200010601     C***
037300010601
037400010601
037500010601
037600010330
037700010330
037800010330
037900990920      /TITLE Invio dei dati al punto operativo.
038000000613     C     invio         BEGSR
038100990920     C*
038200990920     C                   reset                   dscmz
038300010601     C                   move      invfil        cmzdst
038400990920     C                   eval      cmzfld = vlrfou
038500990920     C                   eval      cmzmbd = vlrhdl
038600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
038700000710     C                   if        prmfir = *blanks
038800121107     C                   eval      cmzfla = 'FIVAD00F'
038900121107     C                   eval      cmzmba = 'FIVAD00F'
039000000710     C                   else
039100000710     C                   eval      cmzfla = prmfir
039200000710     C                   eval      cmzmba = prmfir
039300000710     C                   endif
039400990920     C                   eval      cmznrr = *zeros
039500990920     C                   move      §ctrok        cmznrr
039600021003     C                   eval      cmzlba = vlrfl1
039700990920     C                   call(e)   'TIS711C'
039800990920     C                   parm                    dscmz
039900990921     C                   parm      *blanks       esito
040000990920     C                   if        %error
040100990920     C                             or cmzerr = '1'
040200990921     C                             or esito  = '1'
040300000710     C                   eval      wrkesito = '3'
040400990920     C                   endif
040500990920     C*
040600000613     C                   ENDSR
040700990910
040800010601
040900010601
041000010601
041100010601
041200010601      /TITLE Invio dei dati al punto operativo.
041300010601     C     opeini        BEGSR
041400010601     C*
041500010601     C* Inizializzo flag e contatori operativi
041600010601     C                   movel     '0'           flgGiro           1
041700010601     C                   movel     '0'           flgMulti          1
041800010601     C                   movel     '1'           flgStato          1
041900010615     C                   movel     '0'           flgOk             1
042000010601     C                   z-add     *zeros        cntNonEl         10 0
042100010601     C                   z-add     *zeros        depfil            3 0
042200010601     C                   z-add     *zeros        invfil            3 0
042300030407     C*
042400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
042500030407     C                   if        %subst(vlrppt:1:1) = 'S'
042600030407     C                   seton                                        50
042700030407     C                   else
042800030407     C                   setoff                                       50
042900030407     C                   endif
043000010601     C*
043100010601     C                   ENDSR
043200010601     C***
043300010601
043400051212
043500051212
043600051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
043700051212     C     exeBon        BEGSR
043800051212     C*
043900051212     C* Imposto i parametri x il *pgm d "bonifica"
044000051212     C                   EVAL      ElencoChar = *blanks
044100051212     C                   EVAL      TipoElenco = *blanks
044200051212     C                   EVAL      CharSost   = *blanks
044300051212     C                   EVAL      UpperCase  = '1'
044400051212     C                   EVAL      ChkNull    = '1'
044500051212     C                   EVAL      CharNull   = *blanks
044600051212     C*
044700051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
044800051212     C                   EVAL      TxtInOut   = VADCDP
044900051212     C                   EVAL      EsitoBon   = *blanks
045000051212     C*
045100051212     C                   CALL      'XCHKCHAR'
045200051212     C                   PARM                    TxtInOut
045300051212     C                   PARM                    ElencoChar
045400051212     C                   PARM                    TipoElenco
045500051212     C                   PARM                    CharSost
045600051212     C                   PARM                    UpperCase
045700051212     C                   PARM                    ChkNull
045800051212     C                   PARM                    CharNull
045900051212     C                   PARM                    EsitoBon
046000051212     C*
046100051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
046200051212     C                   IF        EsitoBon = '1'
046300051212     C                   EVAL      VADCDP = TxtInOut
046400051212     C                   ENDIF
046500051212     C*
046600051212     C                   ENDSR
046700010601
046800010601
046900010330
047000010330
047100000613     C     *inzsr        BEGSR
047200990910     C*
047300990910     C     *entry        plist
047400990920     C                   parm                    tivlrds
047500990921     C                   parm      wrkesito      esito
047600000724     C                   parm                    prmlit
047700000710     C                   parm                    prmfir
047800010330     C*
047900010330     C* CALCOLA LA DATA CORRENTE
048000120704     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
048100120704     C                   eval      datcor = %dec(%date() : *ISO)
048200000613     C*
048300000613     C                   ENDSR
