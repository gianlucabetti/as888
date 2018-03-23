000100051018      /TITLE Upload via Internet: traduzione in FNVAR00R.
000200990915     H dftactgrp(*yes)
000300990915
000400990915     Ftivin00r  uF   E             DISK    usropn
000500051108     FFNVAR00r  O    E             DISK    commit
000600990915
000700000724     D*------------
000800051017     D tibs55ds      e ds                  inz
000900051017     D kpjba         e ds                  inz
001000990915     D psds           sds
001100990915     D  procname         *PROC
001200990921     D tivlrds       e ds                  extname(tivlr00f)
001300990915     D esito           s              1
001400000724     D prmlit          s             10
001500000710     D prmfir          s             10
001600990921     D wrkesito        s                   like(esito)
001700010604     D*------------------
001800010604     D* DS "XSRDA8" - CONTROLLA DATA (8)
001900010604     D*------------------
002000010604     D WLBDA8          DS                  INZ
002100010604     D  G08DAT                 1      8  0
002200010604     D  G08INV                 9     16  0
002300010604     D  G08ERR                17     17
002400010604     D  G08TGI                18     22  0
002500010604     D*
002600990915
002700990921     C                   reset                   esito
002800990921     C                   reset                   wrkesito
002900010531     C*
003000010531     C                   exsr      opeini
003100010531     C                   exsr      rwvar
003200010531     C*
003300010531     C                   seton                                        lr
003400010531
003500010531
003600010531
003700010531
003800010531     C*--------------------------------------------------------
003900010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004000010531     C*--------------------------------------------------------
004100010531     C     PREELA        BEGSR
004200010531     C*
004300010531     C* SE OCCORRE SPEDIRE IN FILIALE
004400010531     C                   if        invfil <> *zeros and
004500010531     C                             flgGiro = '0'
004600010531     C*
004700010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
004800010531     C                   eval      flgGiro = '1'
004900010531     C*
005000010531     C                   endif
005100010531     C*
005200010531     C                   ENDSR
005300010531     C***
005400010531
005500010531
005600010531
005700010531
005800010531
005900010531
006000010531
006100010531
006200010531
006300010531     C*--------------------------------------------------------
006400010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006500010531     C*--------------------------------------------------------
006600010531     C     ENDELA        BEGSR
006700000616     C*
006800010531     C                   ENDSR
006900010531     C***
007000010531
007100010531
007200120306
007300010531
007400990915
007500910830     C*--------------------------------------------------------
007600941229     C* RWVAR   LEGGE FNVARAAR E SCRIVE FNVAR00R              *
007700910830     C*--------------------------------------------------------
007800941229     C     RWVAR         BEGSR
007900990915     C*
008000990915     C                   if        not %open(tivin00r)
008100990915     C                   open      tivin00r
008200990915     C                   endif
008300990915     C*
008400010601     C                   MOVE      *ZERO         §CTROK
008500010601     C                   MOVE      *ZERO         §CTRNO
008600010604     C                   MOVE      *ZERO         §CTRMO
008700921023     C                   DO        *HIVAL
008800010618     C*
008900990915     C                   READ      tivin00r                               70
009000010618     C*
009100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009200010618     C                   if        vinflg = '1'
009300010618     C                   eval      flgOk = '1'
009400010618     C                   endif
009500010618     C*
009600000905     C                   if        vindta > *blanks
009700990915     C*
009800010601     C                   if        *in70 = *off and
009900990915     C                             (vinflg = *blanks
010000010711     C                              or vinflg = '0'
010100010711     C                              or vinflg = '2')
010200990915     C*
010300010531     C                   clear                   FNVAR000
010400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010500010711     C                   if        vinflg = *blanks or vinflg = '0'
010600010711     C                   clear                   vinmsg
010700010711     C                   endif
010800010711     C*
010900010604     C                   exsr      inzvar
011000010604     C                   exsr      defcam
011100010604     C                   exsr      impvar
011200070115     C*
011300070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011400070115     C                   eval      depfil = VARLNP
011500070115     C*
011600070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
011700070115     C* cliente
011800070507     C                   if        VARCCM = 0893943 OR
011900070507     C                             VARCCM = 0894169
012000070115     C                   eval      depfil = 050
012100070115     C                   eval      invfil = 050
012200070115     C                   endif
012300070507     C                   if        VARCCM = 0893942 OR
012400070507     C                             VARCCM = 0894168
012500070115     C                   eval      depfil = 005
012600070115     C                   eval      invfil = 005
012700070115     C                   endif
012800010604     C*
012900010604     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013000010604     C                   exsr      repfil
013100010604     C                   if        depfil = invfil
013200010604     C*
013300010604     C                   exsr      PREELA
013400010604     C*
013500010604     C* Ebbene...
013600010604     C  N31              ADD       1             §CTROK            7 0
013700010604     C   32              ADD       1             §CTRMO            7 0
013800010604     C   31              ADD       1             §CTRNO            7 0
013900010604     C  N31              WRITE     FNVAR000
014000010604     C*
014100010604     C                   if        *in31 = *off and
014200010604     C                             *in32 = *off
014300010604     C                   eval      vinflg = '1'
014400010604     C                   else
014500010604     C                   eval      vinflg = '2'
014600010604     C                   endif
014700010604     C                   endif
014800010604     C                   endif
014900010604     C*
015000010604     C                   else
015100010604     C                   eval      vinflg = '1'
015200010604     C                   endif
015300000905     C*
015400000905     C  N70              update    tivin000
015500991022     C*
015600991022     C  N70              ENDdo
015700990915     C*
015800010531     C*
015900010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016000010531     C                   if        cntNonEl = *zeros or
016100010531     C                             flgMulti = '0'
016200010601     C* Se non ci sono record con errori ...
016300010601     C                   if        §ctrno = 0 and
016400010604     C                             §ctrmo = 0 and
016500010601     C                             flgStato <> '2'
016600010601     C* ... restituisco esito OK.
016700010601     C                   eval      wrkesito = '0'
016800010601     C                   else
016900010601     C                   if        §ctrok > 0
017000010601     C                   eval      wrkesito = '1'
017100010601     C                   else
017200010615     C                   if        flgOk = '0'
017300010615     C                   eval      wrkesito = '2'
017400010615     C                   else
017500010615     C                   eval      wrkesito = '6'
017600010615     C                   endif
017700010601     C                   endif
017800010601     C                   endif
017900010531     C                   else
018000010531     C                   eval      wrkesito = '9'
018100010531     C                   endif
018200990915     C*
018300990915     C                   if        %open(tivin00r)
018400990915     C                   close     tivin00r
018500990915     C                   endif
018600010531     C*
018700010531     C                   if        vlrpoi <> 999
018800010531     C                   eval      invfil = vlrpoi
018900010531     C                   endif
019000990921     C*
019100010531     C                   if        invfil > *zeros
019200051108     C                   COMMIT
019300990921     C                   exsr      invio
019400000710     C                   endif
019500010531     C*
019600010612     C                   if        flgGiro = '1'
019700010612     C                   exsr      endela
019800010612     C                   endif
019900010612     C*
020000010531     C                   ENDSR
020100010531     C***
020200010531
020300010531
020400010604
020500010604     C*----------------------------------------------------*
020600010604     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020700010604     C*----------------------------------------------------*
020800010604     C     INZVAR        BEGSR
020900010604     C*
021000010604     C*
021100010604     C                   ENDSR
021200010604     C*----------------------------------------------------*
021300010604     C*  IMPOSTAZIONE CAMPI COSTANTI
021400010604     C*----------------------------------------------------*
021500010604     C     DEFCAM        BEGSR
021600010604     C*
021700010604     C                   CLEAR                   FNVAR000
021800010604     C*
021900010604     C                   ENDSR
022000010604     C*----------------------------------------------------*
022100010604     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAR)
022200010604     C*----------------------------------------------------*
022300010604     C     IMPVAR        BEGSR
022400010604     C*
022500010604     C                   SETOFF                                       3132
022600010604     C                   MOVEL     datcor        VARAAS
022700010604     C*
022800010604     C* Reperimento campi ALFA
022900010604     C                   EVAL      VARDMC=%trim(%subst(vindta:34:50))
023000030219     C*
023100030219     C                   EVAL      PiStr=%trim(%subst(vindta:1:4))
023200030219     C                   EXSR      CHKNUM
023300030219     C                   IF        PiInt=*on
023400030219     C                   Z-ADD     PiVal         VARAAS
023500030219     C                   ELSE
023600030219     C                   SETON                                        31
023700030219     C                   EVAL      vinmsg = %trimr(vinmsg)
023800030219     C                             + ' ' + 'VARAAS'
023900030219     C                   ENDIF
024000010604     C*
024100010604     C                   EVAL      PiStr=%trim(%subst(vindta:5:3))
024200010604     C                   EXSR      CHKNUM
024300010604     C                   IF        PiInt=*on
024400010604     C                   Z-ADD     PiVal         VARLNP
024500010604     C                   ELSE
024600010604     C                   SETON                                        31
024700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
024800010604     C                             + ' ' + 'VARLNP'
024900010604     C                   ENDIF
025000010604     C*
025100010604     C                   EVAL      PiStr=%trim(%subst(vindta:8:2))
025200010604     C                   EXSR      CHKNUM
025300010604     C                   IF        PiInt=*on
025400010604     C                   Z-ADD     PiVal         VARNRS
025500010604     C                   ELSE
025600010604     C                   SETON                                        31
025700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
025800010604     C                             + ' ' + 'VARNRS'
025900010604     C                   ENDIF
026000010604     C*
026100010604     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
026200010604     C                   EXSR      CHKNUM
026300010604     C                   IF        PiInt=*on
026400010604     C                   Z-ADD     PiVal         VARNSP
026500010604     C                   ELSE
026600010604     C                   SETON                                        31
026700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
026800010604     C                             + ' ' + 'VARNSP'
026900010604     C                   ENDIF
027000010604     C*
027100010604     C                   EVAL      PiStr=%trim(%subst(vindta:17:4))
027200010604     C                   EXSR      CHKNUM
027300010604     C                   IF        PiInt=*on
027400010604     C                   Z-ADD     PiVal         VARAGC
027500010604     C                   ELSE
027600010604     C                   SETON                                        31
027700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
027800010604     C                             + ' ' + 'VARAGC'
027900010604     C                   ENDIF
028000010604     C*
028100010604     C                   EVAL      PiStr=%trim(%subst(vindta:21:3))
028200010604     C                   EXSR      CHKNUM
028300010604     C                   IF        PiInt=*on
028400010604     C                   Z-ADD     PiVal         VARFGC
028500010604     C                   ELSE
028600010604     C                   SETON                                        31
028700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
028800010604     C                             + ' ' + 'VARFGC'
028900010604     C                   ENDIF
029000010604     C*
029100010604     C                   EVAL      PiStr=%trim(%subst(vindta:24:7))
029200010604     C                   EXSR      CHKNUM
029300010604     C                   IF        PiInt=*on
029400010604     C                   Z-ADD     PiVal         VARNGC
029500010604     C                   ELSE
029600010604     C                   SETON                                        31
029700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
029800010604     C                             + ' ' + 'VARNGC'
029900010604     C                   ENDIF
030000010604     C*
030100010604     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
030200010604     C                   EXSR      CHKNUM
030300010604     C                   IF        PiInt=*on
030400010604     C                   Z-ADD     PiVal         VARNPR
030500010604     C                   ELSE
030600010604     C                   SETON                                        31
030700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
030800010604     C                             + ' ' + 'VARNPR'
030900010604     C                   ENDIF
031000010604     C*
031100010604     C                   EVAL      PiStr=%trim(%subst(vindta:84:8))
031200010604     C                   EXSR      CHKNUM
031300010604     C                   IF        PiInt=*on
031400010604     C                   Z-ADD     PiVal         VARDTR
031500010604     C                   ELSE
031600010604     C                   SETON                                        31
031700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
031800010604     C                             + ' ' + 'VARDTR'
031900010604     C                   ENDIF
032000010604     C*
032100010604     C                   EVAL      PiStr=%trim(%subst(vindta:92:7))
032200010604     C                   EXSR      CHKNUM
032300010604     C                   IF        PiInt=*on
032400010604     C                   Z-ADD     PiVal         VARCCM
032500010604     C                   ELSE
032600010604     C                   SETON                                        31
032700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
032800010604     C                             + ' ' + 'VARCCM'
032900010604     C                   ENDIF
033000010604     C*
033100010604     C                   EVAL      PiStr=%trim(%subst(vindta:99:8))
033200010604     C                   EXSR      CHKNUM
033300010604     C                   IF        PiInt=*on
033400010604     C                   Z-ADD     PiVal         VARDRD
033500010604     C                   ELSE
033600010604     C                   SETON                                        31
033700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
033800010604     C                             + ' ' + 'VARDRD'
033900010604     C                   ENDIF
034000010604     C*
034100010604     C                   ENDSR
034200010604
034300010604
034400010604
034500010604     C*----------------------------------------------------*
034600010604     C*  CONTROLLO NUMERICITA' CAMPI
034700010604     C*----------------------------------------------------*
034800010604     C     CHKNUM        BEGSR
034900010604     C*
035000010604     C                   call(e)   'ISNUMERIC'
035100010604     C                   PARM                    PiStr            30
035200010604     C                   PARM      '.'           PiDecChr          1
035300010604     C                   PARM      *ZEROS        PiVal            30 9
035400010604     C                   PARM      '0'           PiInt             1
035500010604     C                   PARM      '0'           PiNum             1
035600010604     C                   IF        %error
035700010604     C                   EVAL      PiInt=*off
035800010604     C                   ENDIF
035900010604     C*
036000010604     C                   ENDSR
036100010604     C***
036200010604
036300010604
036400010531
036500010531
036600010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036700010531     C     repfil        BEGSR
036800010531     C*
036900010531     C                   if        invfil = *zeros and
037000010531     C                             depfil > *zeros and
037100010613     C                             (vinflg = *blanks or
037200010613     C                              vinflg = *zeros)
037300010531     C
037400010531     C                   eval      invfil = depfil
037500010531     C                   endif
037600010531     C*
037700010531     C                   if        depfil <> invfil and
037800010531     C                             invfil > *zeros
037900010531     C                   eval      flgMulti = '1'
038000010531     C                   if        vinflg = *blanks
038100010531     C                   add       1             cntNonEl
038200010531     C                   endif
038300010531     C                   endif
038400010531     C*
038500010531     C                   if        vinflg = '2'
038600010531     C                   eval      flgStato = '2'
038700010531     C                   endif
038800010531     C*
038900010531     C                   ENDSR
039000010531     C***
039100010531
039200010531
039300010531
039400010531
039500990921
039600990921      /TITLE Invio dei dati al punto operativo.
039700990921     C     invio         begsr
039800051017     C*
039900051017     C                   clear                   tibs55ds
040000051017     C                   move      invfil        i50apo
040100051017     C                   call(e)   'TIBS55R'
040200051017     C                   parm                    tibs55ds
040300051017     C*
040400051018     C                   if        %error or
040500051017     C                             o50err <> *blanks
040600051017     C                   eval      wrkesito = '3'
040700051017     C                   else
040800051018     C*
040900051018     C* Imposto i parametri fondamentali: azione e S.I. in cui sottomettere
041000051018     C                   clear                   KPJBA
041100051018     C                   movel     'LG95'        KCOAZ
041200051018     C                   movel     o50asi        KNSIF
041300051018     C*
041400051018     C* Imposto altri parametri affinchè nn si spacchi la sottomissinoe del job
041500051018     C                   movel     'EDPCEDXXX'   KNMUS
041600051018     C                   z-add     *zeros        KNRAZ
041700051018     C                   movel     'DSP01'       KNMTD
041800051018     C                   z-add     *zeros        KNRVJ
041900051018     C                   z-add     *zeros        KNRJO
042000051018     C                   movel     'N'           KSTJO
042100051018     C                   z-add     *zeros        KTMIN
042200051018     C                   movel     'B'           KTPAZ
042300051018     C                   movel     'RICEZ_VAR'   KNMEB
042400051018     C                   z-add     *zeros        KNMSB
042500051018     C                   z-add     *zeros        KKEYF
042600051018     C                   z-add     *zeros        KRCDS
042700051018     C                   movel     'J'           KEXCN
042800051018     C                   movel     'P5'          KCOJB
042900051018     C***                movel     'KEDI'        KQIEB
043000051018     C***                movel     'KEDI'        KQEBI
043100051018     C                   movel     'N'           KCANC
043200051108     C***                call      'BCH10'
043300051108     C***                parm                    KPJBA
043400051108     C                   call      'FNLG95C2'
043500051108     C                   parm                    KPJBA
043600051017     C                   endif
043700990921     C*
043800990921     C                   endsr
043900010531
044000010531
044100010531
044200010531
044300010531      /TITLE Invio dei dati al punto operativo.
044400010531     C     opeini        BEGSR
044500010531     C*
044600010531     C* Inizializzo flag e contatori operativi
044700010531     C                   movel     '0'           flgGiro           1
044800010531     C                   movel     '0'           flgMulti          1
044900010531     C                   movel     '1'           flgStato          1
045000010615     C                   movel     '0'           flgOk             1
045100010531     C                   z-add     *zeros        cntNonEl         10 0
045200010531     C                   z-add     *zeros        depfil            3 0
045300010531     C                   z-add     *zeros        invfil            3 0
045400030407     C*
045500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
045600040311     C                   if        %subst(vlrppt:1:1) = 'S'
045700040311     C                   seton                                        50
045800030407     C                   else
045900040311     C                   setoff                                       50
046000030407     C                   endif
046100010531     C*
046200010531     C                   ENDSR
046300010531     C***
046400051108
046500051108
046600051108
046700051108      /TITLE Gestione errore generico
046800051108     C     *pssr         BEGSR
046900051108     C*
047000051108     C* Se errore => emetto operazione d ROLLBACK
047100051108     C                   ROLBK
047200051108     C*
047300051108     C                   ENDSR     '*CANCL'
047400051108     C***
047500010531
047600010531
047700010531
047800990915
047900000613     C     *inzsr        BEGSR
048000990915     C*
048100990915     C     *entry        plist
048200990921     C                   parm                    tivlrds
048300990921     C                   parm      wrkesito      esito
048400000724     C                   parm                    prmlit
048500000710     C                   parm                    prmfir
048600010604     C*
048700010604     C* CALCOLA LA DATA CORRENTE
048800010604     C                   time                    wn14             14 0
048900010604     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
049000010604     C                   z-add     wn8           g08dat
049100010604     C                   z-add     *zeros        g08inv
049200010604     C                   movel     '0'           g08err
049300010604     C                   call      'XSRDA8'
049400010604     C                   parm                    wlbda8
049500010604     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049600000613     C*
049700000613     C                   ENDSR
049800000613     C***
