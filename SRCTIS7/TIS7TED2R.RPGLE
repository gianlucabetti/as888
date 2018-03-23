000100050613      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
001900050613     D wNomeFile       s             30    INZ(*blanks)
002000050613     D wCMR            s             35    INZ(*blanks)
002100050613     D wPosDaA         s              2    INZ(*blanks)
002200050613     D wPosDa          s              2  0 INZ(*zeros)
002300050613     D wLungA          s              2    INZ(*blanks)
002400050613     D wLung           s              2  0 INZ(*zeros)
002500010330     D*------------------
002600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002700010330     D*------------------
002800010330     D WLBDA8          DS                  INZ
002900010330     D  G08DAT                 1      8  0
003000010330     D  G08INV                 9     16  0
003100010330     D  G08ERR                17     17
003200010330     D  G08TGI                18     22  0
003300000313     D*
003400051212
003500051212     D TxtInOut        S           2048
003600051212     D ElencoChar      S            256
003700051212     D TipoElenco      S              1
003800051212     D CharSost        S              1
003900051212     D UpperCase       S              1
004000051212     D ChkNull         S              1
004100051212     D CharNull        S              1
004200051212     D EsitoBon        S              1
004300010330
004400010330
004500010330
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010601     C                   exsr      rwvad
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900010601     C*
007000010601     C                   endif
007100010601     C*
007200010601     C                   ENDSR
007300010601     C***
007400010601
007500010601
007600010601
007700010601
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008300010601     C*--------------------------------------------------------
008400010601     C     ENDELA        BEGSR
008500000616     C*
008600010601     C                   ENDSR
008700010601     C***
008800000613
008900010330
009000010601
009100010601
009200010601
009300010330     C*--------------------------------------------------------
009400050613     C* RWVAD   LEGGE tivin00r E SCRIVE EDIVADWF              *
009500010330     C*--------------------------------------------------------
009600010330     C     RWVAD         BEGSR
009700010330     C*
009800050613     C*
009900050613     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
010000050613     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
010100050613     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
010200050613     C                   if        *in60 = *on
010300010330     C                   if        not %open(tivin00r)
010400010330     C                   open      tivin00r
010500010330     C                   endif
010600050613     C                   if        not %open(edivadwr)
010700050613     C                   open      edivadwr
010800010330     C                   endif
010900010330     C*
011000010604     C                   clear                   §CTROK
011100010604     C                   clear                   §CTRMO
011200010604     C                   clear                   §CTRNO
011300010330     C*
011400010330     C                   DO        *HIVAL
011500010330     C*
011600010330     C                   READ      tivin00r                               70
011700010618     C*
011800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011900010618     C                   if        vinflg = '1'
012000010618     C                   eval      flgOk = '1'
012100010618     C                   endif
012200010618     C*
012300010330     C                   if        vindta > *blanks
012400010330     C                   add       1             rrnum
012500010330     C*
012600010601     C                   if        *in70 = *off and
012700010330     C                             (vinflg = *blanks
012800010330     C                              or vinflg = '0'
012900010330     C                              or vinflg = '2')
013000010330     C*
013100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013200010711     C                   if        vinflg = *blanks or vinflg = '0'
013300010711     C                   clear                   vinmsg
013400010711     C                   endif
013500010601     C*
013600010330     C                   exsr      inzvar
013700010330     C                   exsr      defcam
013800010330     C                   exsr      impvad
013900050613     C*
014000050613     C* VALORIZZO CAMPI RELATIVI AL "CMR"
014100050613     C                   EVAL      VADCMR = wCMR
014200050613     C                   EVAL      VADCNT = 1
014300010601     C*
014400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014500101209     C                   eval      depfil = VADFGS
014600010601     C                   exsr      repfil
014700010601     C                   if        depfil = invfil
014800021014     C                   if        vlrpoi = 999
014900101209     C                   MOVE(P)   invfil        VADFGS
015000021014     C                   else
015100021014     C                   MOVE(P)   vlrpoi        VADFGS
015200021014     C                   endif
015300010601     C*
015400010601     C                   exsr      PREELA
015500051212
015600051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
015700051212     c                   exsr      exeBon
015800051212
015900010601     C*
016000010604     C* Ebbene...
016100010604     C*
016200010604     C  N31              ADD       1             §CTROK            7 0
016300010604     C   32              ADD       1             §CTRMO            7 0
016400010604     C   31              ADD       1             §CTRNO            7 0
016500050613     C  N31              WRITE     EDIVAD00
016600010604     C*
016700010604     C                   if        *in31 = *off and
016800010604     C                             *in32 = *off
016900010604     C                   eval      vinflg = '1'
017000010604     C                   else
017100010604     C                   eval      vinflg = '2'
017200010604     C                   endif
017300010604     C                   endif
017400010601     C*
017500010604     C                   endif
017600010604     C*
017700010330     C                   else
017800010330     C                   eval      vinflg = '1'
017900010330     C                   endif
018000010601     C*
018100010601     C  N70              update    tivin000
018200010330     C*
018300010330     C  N70              ENDdo
018400010601     C
018500010601     C*
018600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018700010601     C                   if        cntNonEl = *zeros or
018800010601     C                             flgMulti = '0'
018900010330     C* Se non ci sono record con errori ...
019000010601     C                   if        §ctrno = 0 and
019100010604     C                             §ctrmo = 0 and
019200010601     C                             flgStato <> '2'
019300010330     C* ... restituisco esito OK.
019400010330     C                   eval      wrkesito = '0'
019500010330     C                   else
019600010330     C                   if        §ctrok > 0
019700010330     C                   eval      wrkesito = '1'
019800010330     C                   else
019900010615     C                   if        flgOk = '0'
020000010615     C                   eval      wrkesito = '2'
020100010615     C                   else
020200010615     C                   eval      wrkesito = '6'
020300010615     C                   endif
020400010330     C                   endif
020500010330     C                   endif
020600010601     C                   else
020700010601     C                   eval      wrkesito = '9'
020800010601     C                   endif
020900050613     C*
021000050613     C                   else
021100050613     C                   eval      wrkesito = '8'
021200050613     C                   eval      flgGiro = '1'
021300050613     C                   endif
021400010330     C*
021500010330     C                   if        %open(tivin00r)
021600010330     C                   close     tivin00r
021700010330     C                   endif
021800050613     C                   if        %open(edivadwr)
021900050613     C                   close     edivadwr
022000010330     C                   endif
022100010601     C*
022200010601     C                   if        vlrpoi <> 999
022300010601     C                   eval      invfil = vlrpoi
022400010601     C                   endif
022500010330     C*
022600010330     C                   if        §ctrok > 0
022700010601     C                             and invfil > *zeros
022800010330     C                   exsr      invio
022900010330     C                   endif
023000010612     C*
023100010612     C                   if        flgGiro = '1'
023200010612     C                   exsr      endela
023300010612     C                   endif
023400010330     C*
023500010330     C                   ENDSR
023600010330     C***
023700010330
023800010601
023900010601
024000010601
024100010601
024200010330     C*----------------------------------------------------*
024300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024400010330     C*----------------------------------------------------*
024500010330     C     INZVAR        BEGSR
024600010330     C*
024700010330     C*
024800010330     C                   ENDSR
024900010330     C*----------------------------------------------------*
025000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
025100010330     C*----------------------------------------------------*
025200010330     C     DEFCAM        BEGSR
025300010330     C*
025400050613     C                   CLEAR                   EDIVAD00
025500010330     C*
025600010330     C                   ENDSR
025700010330     C*----------------------------------------------------*
025800050613     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAD)
025900010330     C*----------------------------------------------------*
026000010330     C     IMPVAD        BEGSR
026100010330     C*
026200010604     C                   SETOFF                                       3132
026300010330     C*
026400010330     C* Reperimento campi ALFA
026500010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
026600101209     C                   EVAL      VADCDP=%trim(%subst(vindta:28:15))
026700101209     C*
026800101209     C                   EVAL      PiStr=%trim(%subst(vindta:2:3))
026900101209     C                   EXSR      CHKNUM
027000101209     C                   IF        PiInt=*on
027100101209     C                   Z-ADD     PiVal         VADFGS
027200101209     C                   ELSE
027300101209     C                   SETON                                        31
027400101209     C                   EVAL      vinmsg = %trimr(vinmsg)
027500101209     C                             + ' ' + 'VADFGS'
027600101209     C                   ENDIF
027700010330     C*
027800101209     C                   EVAL      PiStr=%trim(%subst(vindta:5:7))
027900010330     C                   EXSR      CHKNUM
028000010330     C                   IF        PiInt=*on
028100010330     C                   Z-ADD     PiVal         VADCCM
028200010330     C                   ELSE
028300010604     C                   SETON                                        31
028400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
028500010330     C                             + ' ' + 'VADCCM'
028600010330     C                   ENDIF
028700010330     C*
028800101209     C                   EVAL      PiStr=%trim(%subst(vindta:12:3))
028900010330     C                   EXSR      CHKNUM
029000010330     C                   IF        PiInt=*on
029100010330     C                   Z-ADD     PiVal         VADLNP
029200010330     C                   ELSE
029300010604     C                   SETON                                        31
029400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
029500021010     C                             + ' ' + 'VADLNP'
029600010330     C                   ENDIF
029700050104     C*
029800101209     C                   IF        %subst(vindta:15:4) <> *blanks
029900101209     C                   EVAL      PiStr=%trim(%subst(vindta:15:4))
030000050104     C                   EXSR      CHKNUM
030100050104     C                   IF        PiInt=*on
030200050104     C                   Z-ADD     PiVal         VADAAS
030300050104     C                   ELSE
030400050104     C                   SETON                                        31
030500050104     C                   EVAL      vinmsg = %trimr(vinmsg)
030600050104     C                             + ' ' + 'VADAAS'
030700050104     C                   ENDIF
030800050104     C                   ELSE
030900050104     C                   MOVEL     datcor        VADAAS
031000050104     C                   ENDIF
031100010330     C*
031200101209     C                   EVAL      PiStr=%trim(%subst(vindta:19:2))
031300010330     C                   EXSR      CHKNUM
031400010330     C                   IF        PiInt=*on
031500010330     C                   Z-ADD     PiVal         VADNRS
031600010330     C                   ELSE
031700010604     C                   SETON                                        32
031800010604     C                   Z-ADD     *zeros        VADNRS
031900010330     C                   EVAL      vinmsg = %trimr(vinmsg)
032000010330     C                             + ' ' + 'VADNRS'
032100010330     C                   ENDIF
032200010330     C*
032300101209     C                   EVAL      PiStr=%trim(%subst(vindta:21:7))
032400010330     C                   EXSR      CHKNUM
032500010330     C                   IF        PiInt=*on
032600010330     C                   Z-ADD     PiVal         VADNSP
032700010330     C                   ELSE
032800010604     C                   SETON                                        32
032900010604     C                   Z-ADD     *zeros        VADNSP
033000010330     C                   EVAL      vinmsg = %trimr(vinmsg)
033100010330     C                             + ' ' + 'VADNSP'
033200010330     C                   ENDIF
033300010330     C*
033400101209     C                   EVAL      PiStr=%trim(%subst(vindta:43:5))
033500010330     C                   EXSR      CHKNUM
033600010330     C                   IF        PiInt=*on
033700010330     C                   Z-ADD     PiVal         VADNCL
033800010330     C                   ELSE
033900010604     C                   SETON                                        32
034000010604     C                   Z-ADD     *zeros        VADNCL
034100010330     C                   EVAL      vinmsg = %trimr(vinmsg)
034200010330     C                             + ' ' + 'VADNCL'
034300010330     C                   ENDIF
034400010330     C*
034500101209     C                   EVAL      PiStr=%trim(%subst(vindta:48:7))
034600010330     C                   EXSR      CHKNUM
034700010330     C                   IF        PiInt=*on
034800010330     C                   Z-ADD     PiVal         VADNCD
034900010330     C                   ELSE
035000010604     C                   SETON                                        32
035100010604     C                   Z-ADD     *zeros        VADNCD
035200010330     C                   EVAL      vinmsg = %trimr(vinmsg)
035300010330     C                             + ' ' + 'VADNCD'
035400010330     C                   ENDIF
035500010330     C*
035600101209     C                   EVAL      PiStr=%trim(%subst(vindta:55:7))
035700010330     C                   EXSR      CHKNUM
035800010330     C                   IF        PiInt=*on
035900010330     C                   Z-ADD     PiVal         VADNCA
036000010330     C                   ELSE
036100010604     C                   SETON                                        32
036200010604     C                   Z-ADD     *zeros        VADNCA
036300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
036400010330     C                             + ' ' + 'VADNCA'
036500010330     C                   ENDIF
036600010330     C*
036700010330     C                   ENDSR
036800990920
036900010601
037000010601
037100010330
037200010330
037300010330     C*----------------------------------------------------*
037400010330     C*  CONTROLLO NUMERICITA' CAMPI
037500010330     C*----------------------------------------------------*
037600010330     C     CHKNUM        BEGSR
037700010330     C*
037800010330     C                   call(e)   'ISNUMERIC'
037900010330     C                   PARM                    PiStr            30
038000010330     C                   PARM      '.'           PiDecChr          1
038100010330     C                   PARM      *ZEROS        PiVal            30 9
038200010330     C                   PARM      '0'           PiInt             1
038300010330     C                   PARM      '0'           PiNum             1
038400010330     C                   IF        %error
038500010330     C                   EVAL      PiInt=*off
038600010330     C                   ENDIF
038700010330     C*
038800010330     C                   ENDSR
038900010330     C***
039000010330
039100010601
039200010601
039300010601
039400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
039500010601     C     repfil        BEGSR
039600010601     C*
039700010601     C                   if        invfil = *zeros and
039800010601     C                             depfil > *zeros and
039900010613     C                             (vinflg = *blanks or
040000010613     C                              vinflg = *zeros)
040100010601     C
040200010601     C                   eval      invfil = depfil
040300010601     C                   endif
040400010601     C*
040500010601     C                   if        depfil <> invfil and
040600010601     C                             invfil > *zeros
040700010601     C                   eval      flgMulti = '1'
040800010601     C                   if        vinflg = *blanks
040900010601     C                   add       1             cntNonEl
041000010601     C                   endif
041100010601     C                   endif
041200010601     C*
041300010601     C                   if        vinflg = '2'
041400010601     C                   eval      flgStato = '2'
041500010601     C                   endif
041600010601     C*
041700010601     C                   ENDSR
041800010601     C***
041900010601
042000010601
042100010601
042200010330
042300010330
042400010330
042500990920      /TITLE Invio dei dati al punto operativo.
042600000613     C     invio         BEGSR
042700990920     C*
042800990920     C                   reset                   dscmz
042900010601     C                   move      invfil        cmzdst
043000990920     C                   eval      cmzfld = vlrfou
043100990920     C                   eval      cmzmbd = vlrhdl
043200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
043300000710     C                   if        prmfir = *blanks
043400050613     C                   eval      cmzfla = 'EDIVAD0F'
043500050613     C                   eval      cmzmba = 'EDIVAD0F'
043600000710     C                   else
043700000710     C                   eval      cmzfla = prmfir
043800000710     C                   eval      cmzmba = prmfir
043900000710     C                   endif
044000990920     C                   eval      cmznrr = *zeros
044100990920     C                   move      §ctrok        cmznrr
044200021003     C                   eval      cmzlba = vlrfl1
044300990920     C                   call(e)   'TIS711C'
044400990920     C                   parm                    dscmz
044500990921     C                   parm      *blanks       esito
044600990920     C                   if        %error
044700990920     C                             or cmzerr = '1'
044800990921     C                             or esito  = '1'
044900000710     C                   eval      wrkesito = '3'
045000990920     C                   endif
045100990920     C*
045200000613     C                   ENDSR
045300990910
045400010601
045500010601
045600010601
045700010601
045800010601      /TITLE Invio dei dati al punto operativo.
045900010601     C     opeini        BEGSR
046000010601     C*
046100010601     C* Inizializzo flag e contatori operativi
046200010601     C                   movel     '0'           flgGiro           1
046300010601     C                   movel     '0'           flgMulti          1
046400010601     C                   movel     '1'           flgStato          1
046500010615     C                   movel     '0'           flgOk             1
046600010601     C                   z-add     *zeros        cntNonEl         10 0
046700010601     C                   z-add     *zeros        depfil            3 0
046800010601     C                   z-add     *zeros        invfil            3 0
046900030407     C*
047000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
047100030407     C                   if        %subst(vlrppt:1:1) = 'S'
047200030407     C                   seton                                        50
047300030407     C                   else
047400030407     C                   setoff                                       50
047500030407     C                   endif
047600010601     C*
047700010601     C                   ENDSR
047800010601     C***
047900010601
048000051212
048100051212
048200051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
048300051212     C     exeBon        BEGSR
048400051212     C*
048500051212     C* Imposto i parametri x il *pgm d "bonifica"
048600051212     C                   EVAL      ElencoChar = *blanks
048700051212     C                   EVAL      TipoElenco = *blanks
048800051212     C                   EVAL      CharSost   = *blanks
048900051212     C                   EVAL      UpperCase  = '1'
049000051212     C                   EVAL      ChkNull    = '1'
049100051212     C                   EVAL      CharNull   = *blanks
049200051212     C*
049300051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
049400051212     C                   EVAL      TxtInOut   = VADCDP
049500051212     C                   EVAL      EsitoBon   = *blanks
049600051212     C*
049700051212     C                   CALL      'XCHKCHAR'
049800051212     C                   PARM                    TxtInOut
049900051212     C                   PARM                    ElencoChar
050000051212     C                   PARM                    TipoElenco
050100051212     C                   PARM                    CharSost
050200051212     C                   PARM                    UpperCase
050300051212     C                   PARM                    ChkNull
050400051212     C                   PARM                    CharNull
050500051212     C                   PARM                    EsitoBon
050600051212     C*
050700051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
050800051212     C                   IF        EsitoBon = '1'
050900051212     C                   EVAL      VADCDP = TxtInOut
051000051212     C                   ENDIF
051100051212     C*
051200051212     C                   ENDSR
051300010601
051400010601
051500010330
051600010330
051700000613     C     *inzsr        BEGSR
051800990910     C*
051900990910     C     *entry        plist
052000990920     C                   parm                    tivlrds
052100990921     C                   parm      wrkesito      esito
052200000724     C                   parm                    prmlit
052300000710     C                   parm                    prmfir
052400050613     C*
052500050613     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
052600050613     C                   move(p)   vlrMSG        wNomeFile
052700050613     C*
052800050613     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
052900050613     C* del codice CMR dal nome del file importato.
053000050613     C                   if        wNomeFile <> *blanks AND
053100050613     C                             vlrppt<>*blanks
053200050613     C                   if        %subst(vlrppt:1:1) = 'S'
053300050613     C                   seton                                        50
053400050613     C                   else
053500050613     C                   setoff                                       50
053600050613     C                   endif
053700050613     C                   eval      wPosDaA = %subst(vlrppt:2:2)
053800050613     C                   move(p)   wPosDaA       wPosDa
053900050613     C                   eval      wLungA  = %subst(vlrppt:4:2)
054000050613     C                   move(p)   wLungA        wLung
054100050613     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
054200050613     C                   seton                                        60
054300050613     C                   else
054400050613     C                   setoff                                       60
054500050613     C                   endif
054600010330     C*
054700010330     C* CALCOLA LA DATA CORRENTE
054800010330     C                   time                    wn14             14 0
054900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055000010330     C                   z-add     wn8           g08dat
055100010330     C                   z-add     *zeros        g08inv
055200010330     C                   movel     '0'           g08err
055300010330     C                   call      'XSRDA8'
055400010330     C                   parm                    wlbda8
055500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055600000613     C*
055700000613     C                   ENDSR
055800000613     C***
