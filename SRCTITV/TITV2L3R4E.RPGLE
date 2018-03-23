000100050613      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050613     FEDIVADwr  O    E             DISK    usropn
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
007000050613     C* RWVAD   LEGGE tivin00r E SCRIVE EDIVADWF              *
007100010330     C*--------------------------------------------------------
007200010330     C     RWVAD         BEGSR
007300010330     C*
007400010330     C                   if        not %open(tivin00r)
007500010330     C                   open      tivin00r
007600010330     C                   endif
007700050613     C                   if        not %open(edivadwr)
007800050613     C                   open      edivadwr
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
011000050613     C*
011100050613     C* VALORIZZO CAMPI RELATIVI AL "CMR"
011200121102     C                   EVAL      VADCMR = '0896769 LOGHISTES SAP'+
011300121102     C                                      ' '+%char(%date() : *ISO)
011400050613     C                   EVAL      VADCNT = 1
011500010601     C*
011600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011700010601     C                   eval      depfil = VADLNP
011800010601     C                   exsr      repfil
011900010601     C                   if        depfil = invfil
012000021014     C                   if        vlrpoi = 999
012100021010     C                   MOVE(P)   invfil        VADFGS
012200021014     C                   else
012300021014     C                   MOVE(P)   vlrpoi        VADFGS
012400021014     C                   endif
012500010601     C*
012600010601     C                   exsr      PREELA
012700051212
012800051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
012900051212     c                   exsr      exeBon
013000051212
013100010601     C*
013200010604     C* Ebbene...
013300010604     C*
013400010604     C  N31              ADD       1             §CTROK            7 0
013500010604     C   32              ADD       1             §CTRMO            7 0
013600010604     C   31              ADD       1             §CTRNO            7 0
013700050613     C  N31              WRITE     EDIVAD00
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
017800050613     C*
017900050613     C                   else
018000050613     C                   eval      wrkesito = '8'
018100050613     C                   eval      flgGiro = '1'
018200050613     C                   endif
018300010330     C*
018400010330     C                   if        %open(tivin00r)
018500010330     C                   close     tivin00r
018600010330     C                   endif
018700050613     C                   if        %open(edivadwr)
018800050613     C                   close     edivadwr
018900010330     C                   endif
019000010601     C*
019100010601     C                   if        vlrpoi <> 999
019200010601     C                   eval      invfil = vlrpoi
019300010601     C                   endif
019400010330     C*
019500010330     C                   if        §ctrok > 0
019600010601     C                             and invfil > *zeros
019700010330     C                   exsr      invio
019800010330     C                   endif
019900010612     C*
020000010612     C                   if        flgGiro = '1'
020100010612     C                   exsr      endela
020200010612     C                   endif
020300010330     C*
020400010330     C                   ENDSR
020500010330     C***
020600010330
020700010601
020800010601
020900010601
021000010601
021100010330     C*----------------------------------------------------*
021200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021300010330     C*----------------------------------------------------*
021400010330     C     INZVAR        BEGSR
021500010330     C*
021600010330     C*
021700010330     C                   ENDSR
021800010330     C*----------------------------------------------------*
021900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022000010330     C*----------------------------------------------------*
022100010330     C     DEFCAM        BEGSR
022200010330     C*
022300050613     C                   CLEAR                   EDIVAD00
022400010330     C*
022500010330     C                   ENDSR
022600010330     C*----------------------------------------------------*
022700050613     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAD)
022800010330     C*----------------------------------------------------*
022900010330     C     IMPVAD        BEGSR
023000010330     C*
023100010604     C                   SETOFF                                       3132
023200010330     C*
023300010330     C* Reperimento campi ALFA
023400010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
023500010330     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
023600010330     C*
023700010330     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
023800010330     C                   EXSR      CHKNUM
023900010330     C                   IF        PiInt=*on
024000010330     C                   Z-ADD     PiVal         VADCCM
024100010330     C                   ELSE
024200010604     C                   SETON                                        31
024300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
024400010330     C                             + ' ' + 'VADCCM'
024500010330     C                   ENDIF
024600010330     C*
024700010330     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
024800010330     C                   EXSR      CHKNUM
024900010330     C                   IF        PiInt=*on
025000010330     C                   Z-ADD     PiVal         VADLNP
025100010330     C                   ELSE
025200010604     C                   SETON                                        31
025300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
025400021010     C                             + ' ' + 'VADLNP'
025500010330     C                   ENDIF
025600050104     C*
025700050104     C                   IF        %subst(vindta:12:4) <> *blanks
025800050104     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
025900050104     C                   EXSR      CHKNUM
026000050104     C                   IF        PiInt=*on
026100050104     C                   Z-ADD     PiVal         VADAAS
026200050104     C                   ELSE
026300050104     C                   SETON                                        31
026400050104     C                   EVAL      vinmsg = %trimr(vinmsg)
026500050104     C                             + ' ' + 'VADAAS'
026600050104     C                   ENDIF
026700050104     C                   ELSE
026800050104     C                   MOVEL     datcor        VADAAS
026900050104     C                   ENDIF
027000010330     C*
027100010330     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
027200010330     C                   EXSR      CHKNUM
027300010330     C                   IF        PiInt=*on
027400010330     C                   Z-ADD     PiVal         VADNRS
027500010330     C                   ELSE
027600010604     C                   SETON                                        32
027700010604     C                   Z-ADD     *zeros        VADNRS
027800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
027900010330     C                             + ' ' + 'VADNRS'
028000010330     C                   ENDIF
028100010330     C*
028200010330     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
028300010330     C                   EXSR      CHKNUM
028400010330     C                   IF        PiInt=*on
028500010330     C                   Z-ADD     PiVal         VADNSP
028600010330     C                   ELSE
028700010604     C                   SETON                                        32
028800010604     C                   Z-ADD     *zeros        VADNSP
028900010330     C                   EVAL      vinmsg = %trimr(vinmsg)
029000010330     C                             + ' ' + 'VADNSP'
029100010330     C                   ENDIF
029200010330     C*
029300010330     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
029400010330     C                   EXSR      CHKNUM
029500010330     C                   IF        PiInt=*on
029600010330     C                   Z-ADD     PiVal         VADNCL
029700010330     C                   ELSE
029800010604     C                   SETON                                        32
029900010604     C                   Z-ADD     *zeros        VADNCL
030000010330     C                   EVAL      vinmsg = %trimr(vinmsg)
030100010330     C                             + ' ' + 'VADNCL'
030200010330     C                   ENDIF
030300010330     C*
030400010330     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
030500010330     C                   EXSR      CHKNUM
030600010330     C                   IF        PiInt=*on
030700010330     C                   Z-ADD     PiVal         VADNCD
030800010330     C                   ELSE
030900010604     C                   SETON                                        32
031000010604     C                   Z-ADD     *zeros        VADNCD
031100010330     C                   EVAL      vinmsg = %trimr(vinmsg)
031200010330     C                             + ' ' + 'VADNCD'
031300010330     C                   ENDIF
031400010330     C*
031500010330     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
031600010330     C                   EXSR      CHKNUM
031700010330     C                   IF        PiInt=*on
031800010330     C                   Z-ADD     PiVal         VADNCA
031900010330     C                   ELSE
032000010604     C                   SETON                                        32
032100010604     C                   Z-ADD     *zeros        VADNCA
032200010330     C                   EVAL      vinmsg = %trimr(vinmsg)
032300010330     C                             + ' ' + 'VADNCA'
032400010330     C                   ENDIF
032500010330     C*
032600010330     C                   ENDSR
032700990920
032800010601
032900010601
033000010330
033100010330
033200010330     C*----------------------------------------------------*
033300010330     C*  CONTROLLO NUMERICITA' CAMPI
033400010330     C*----------------------------------------------------*
033500010330     C     CHKNUM        BEGSR
033600010330     C*
033700010330     C                   call(e)   'ISNUMERIC'
033800010330     C                   PARM                    PiStr            30
033900010330     C                   PARM      '.'           PiDecChr          1
034000010330     C                   PARM      *ZEROS        PiVal            30 9
034100010330     C                   PARM      '0'           PiInt             1
034200010330     C                   PARM      '0'           PiNum             1
034300010330     C                   IF        %error
034400010330     C                   EVAL      PiInt=*off
034500010330     C                   ENDIF
034600010330     C*
034700010330     C                   ENDSR
034800010330     C***
034900010330
035000010601
035100010601
035200010601
035300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
035400010601     C     repfil        BEGSR
035500010601     C*
035600010601     C                   if        invfil = *zeros and
035700010601     C                             depfil > *zeros and
035800010613     C                             (vinflg = *blanks or
035900010613     C                              vinflg = *zeros)
036000010601     C
036100010601     C                   eval      invfil = depfil
036200010601     C                   endif
036300010601     C*
036400010601     C                   if        depfil <> invfil and
036500010601     C                             invfil > *zeros
036600010601     C                   eval      flgMulti = '1'
036700010601     C                   if        vinflg = *blanks
036800010601     C                   add       1             cntNonEl
036900010601     C                   endif
037000010601     C                   endif
037100010601     C*
037200010601     C                   if        vinflg = '2'
037300010601     C                   eval      flgStato = '2'
037400010601     C                   endif
037500010601     C*
037600010601     C                   ENDSR
037700010601     C***
037800010601
037900010601
038000010601
038100010330
038200010330
038300010330
038400990920      /TITLE Invio dei dati al punto operativo.
038500000613     C     invio         BEGSR
038600990920     C*
038700990920     C                   reset                   dscmz
038800010601     C                   move      invfil        cmzdst
038900990920     C                   eval      cmzfld = vlrfou
039000990920     C                   eval      cmzmbd = vlrhdl
039100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
039200000710     C                   if        prmfir = *blanks
039300050613     C                   eval      cmzfla = 'EDIVAD0F'
039400050613     C                   eval      cmzmba = 'EDIVAD0F'
039500000710     C                   else
039600000710     C                   eval      cmzfla = prmfir
039700000710     C                   eval      cmzmba = prmfir
039800000710     C                   endif
039900990920     C                   eval      cmznrr = *zeros
040000990920     C                   move      §ctrok        cmznrr
040100021003     C                   eval      cmzlba = vlrfl1
040200990920     C                   call(e)   'TIS711C'
040300990920     C                   parm                    dscmz
040400990921     C                   parm      *blanks       esito
040500990920     C                   if        %error
040600990920     C                             or cmzerr = '1'
040700990921     C                             or esito  = '1'
040800000710     C                   eval      wrkesito = '3'
040900990920     C                   endif
041000990920     C*
041100000613     C                   ENDSR
041200990910
041300010601
041400010601
041500010601
041600010601
041700010601      /TITLE Invio dei dati al punto operativo.
041800010601     C     opeini        BEGSR
041900010601     C*
042000010601     C* Inizializzo flag e contatori operativi
042100010601     C                   movel     '0'           flgGiro           1
042200010601     C                   movel     '0'           flgMulti          1
042300010601     C                   movel     '1'           flgStato          1
042400010615     C                   movel     '0'           flgOk             1
042500010601     C                   z-add     *zeros        cntNonEl         10 0
042600010601     C                   z-add     *zeros        depfil            3 0
042700010601     C                   z-add     *zeros        invfil            3 0
042800030407     C*
042900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
043000030407     C                   if        %subst(vlrppt:1:1) = 'S'
043100030407     C                   seton                                        50
043200030407     C                   else
043300030407     C                   setoff                                       50
043400030407     C                   endif
043500010601     C*
043600010601     C                   ENDSR
043700010601     C***
043800010601
043900051212
044000051212
044100051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
044200051212     C     exeBon        BEGSR
044300051212     C*
044400051212     C* Imposto i parametri x il *pgm d "bonifica"
044500051212     C                   EVAL      ElencoChar = *blanks
044600051212     C                   EVAL      TipoElenco = *blanks
044700051212     C                   EVAL      CharSost   = *blanks
044800051212     C                   EVAL      UpperCase  = '1'
044900051212     C                   EVAL      ChkNull    = '1'
045000051212     C                   EVAL      CharNull   = *blanks
045100051212     C*
045200051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
045300051212     C                   EVAL      TxtInOut   = VADCDP
045400051212     C                   EVAL      EsitoBon   = *blanks
045500051212     C*
045600051212     C                   CALL      'XCHKCHAR'
045700051212     C                   PARM                    TxtInOut
045800051212     C                   PARM                    ElencoChar
045900051212     C                   PARM                    TipoElenco
046000051212     C                   PARM                    CharSost
046100051212     C                   PARM                    UpperCase
046200051212     C                   PARM                    ChkNull
046300051212     C                   PARM                    CharNull
046400051212     C                   PARM                    EsitoBon
046500051212     C*
046600051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
046700051212     C                   IF        EsitoBon = '1'
046800051212     C                   EVAL      VADCDP = TxtInOut
046900051212     C                   ENDIF
047000051212     C*
047100051212     C                   ENDSR
047200010601
047300010601
047400010330
047500010330
047600000613     C     *inzsr        BEGSR
047700990910     C*
047800990910     C     *entry        plist
047900990920     C                   parm                    tivlrds
048000990921     C                   parm      wrkesito      esito
048100000724     C                   parm                    prmlit
048200000710     C                   parm                    prmfir
048300010330     C*
048400010330     C* CALCOLA LA DATA CORRENTE
048500120704     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
048600120704     C                   eval      datcor = %dec(%date() : *ISO)
048700000613     C*
048800000613     C                   ENDSR
