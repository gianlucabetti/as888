000100051018      /TITLE Upload via Internet: traduzione in FNVAR00R.
000200130108     H DFTACTGRP(*no) ACTGRP(*caller)
000300990915
000400990915     Ftivin00r  uF   E             DISK    usropn
000500051108     FFNVAR00r  O    E             DISK    commit
000600080908
000700080908     D TxtInOut        S           2048
000800080908     D ElencoChar      S            256
000900080908     D TipoElenco      S              1
001000080908     D CharSost        S              1
001100080908     D UpperCase       S              1
001200080908     D ChkNull         S              1
001300080908     D CharNull        S              1
001400080908     D EsitoBon        S              1
001500080908
001600990915
001700000724     D*------------
001800051017     D tibs55ds      e ds                  inz
001900051017     D kpjba         e ds                  inz
002000990915     D psds           sds
002100990915     D  procname         *PROC
002200990921     D tivlrds       e ds                  extname(tivlr00f)
002300990915     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700010604     D*------------------
002800010604     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900010604     D*------------------
003000010604     D WLBDA8          DS                  INZ
003100010604     D  G08DAT                 1      8  0
003200010604     D  G08INV                 9     16  0
003300010604     D  G08ERR                17     17
003400010604     D  G08TGI                18     22  0
003500010604     D*
003600990915
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010531     C*
004000010531     C                   exsr      opeini
004100010531     C                   exsr      rwvar
004200010531     C*
004300010531     C                   seton                                        lr
004400010531
004500010531
004600010531
004700010531
004800010531     C*--------------------------------------------------------
004900010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005000010531     C*--------------------------------------------------------
005100010531     C     PREELA        BEGSR
005200010531     C*
005300010531     C* SE OCCORRE SPEDIRE IN FILIALE
005400010531     C                   if        invfil <> *zeros and
005500010531     C                             flgGiro = '0'
005600010531     C*
005700010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005800010531     C                   eval      flgGiro = '1'
005900010531     C*
006000010531     C                   endif
006100010531     C*
006200010531     C                   ENDSR
006300010531     C***
006400010531
006500010531
006600010531
006700010531
006800010531
006900010531
007000010531
007100010531
007200010531
007300010531     C*--------------------------------------------------------
007400010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010531     C*--------------------------------------------------------
007600010531     C     ENDELA        BEGSR
007700000616     C*
007800010531     C                   ENDSR
007900010531     C***
008000010531
008100010531
008200010531
008300010531
008400990915
008500910830     C*--------------------------------------------------------
008600941229     C* RWVAR   LEGGE FNVARAAR E SCRIVE FNVAR00R              *
008700910830     C*--------------------------------------------------------
008800941229     C     RWVAR         BEGSR
008900990915     C*
009000990915     C                   if        not %open(tivin00r)
009100990915     C                   open      tivin00r
009200990915     C                   endif
009300990915     C*
009400010601     C                   MOVE      *ZERO         §CTROK
009500010601     C                   MOVE      *ZERO         §CTRNO
009600010604     C                   MOVE      *ZERO         §CTRMO
009700921023     C                   DO        *HIVAL
009800010618     C*
009900990915     C                   READ      tivin00r                               70
010000010618     C*
010100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010200010618     C                   if        vinflg = '1'
010300010618     C                   eval      flgOk = '1'
010400010618     C                   endif
010500010618     C*
010600000905     C                   if        vindta > *blanks
010700990915     C*
010800010601     C                   if        *in70 = *off and
010900990915     C                             (vinflg = *blanks
011000010711     C                              or vinflg = '0'
011100010711     C                              or vinflg = '2')
011200990915     C*
011300010531     C                   clear                   FNVAR000
011400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011500010711     C                   if        vinflg = *blanks or vinflg = '0'
011600010711     C                   clear                   vinmsg
011700010711     C                   endif
011800010711     C*
011900010604     C                   exsr      inzvar
012000010604     C                   exsr      defcam
012100010604     C                   exsr      impvar
012200010604     C*
012300010604     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012400010604     C                   eval      depfil = VARLNP
012500010604     C                   exsr      repfil
012600010604     C                   if        depfil = invfil
012700010604     C*
012800010604     C                   exsr      PREELA
012900080908     C*
013000080908     C* "bonifico" i dati di input da eventuali caratteri indesiderati
013100080908     C                   exsr      exeBon
013200010604     C*
013300010604     C* Ebbene...
013400010604     C  N31              ADD       1             §CTROK            7 0
013500010604     C   32              ADD       1             §CTRMO            7 0
013600010604     C   31              ADD       1             §CTRNO            7 0
013700010604     C  N31              WRITE     FNVAR000
013800010604     C*
013900010604     C                   if        *in31 = *off and
014000010604     C                             *in32 = *off
014100010604     C                   eval      vinflg = '1'
014200010604     C                   else
014300010604     C                   eval      vinflg = '2'
014400010604     C                   endif
014500010604     C                   endif
014600010604     C                   endif
014700010604     C*
014800010604     C                   else
014900010604     C                   eval      vinflg = '1'
015000010604     C                   endif
015100000905     C*
015200000905     C  N70              update    tivin000
015300991022     C*
015400991022     C  N70              ENDdo
015500990915     C*
015600010531     C*
015700010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015800010531     C                   if        cntNonEl = *zeros or
015900010531     C                             flgMulti = '0'
016000010601     C* Se non ci sono record con errori ...
016100010601     C                   if        §ctrno = 0 and
016200010604     C                             §ctrmo = 0 and
016300010601     C                             flgStato <> '2'
016400010601     C* ... restituisco esito OK.
016500010601     C                   eval      wrkesito = '0'
016600010601     C                   else
016700010601     C                   if        §ctrok > 0
016800010601     C                   eval      wrkesito = '1'
016900010601     C                   else
017000010615     C                   if        flgOk = '0'
017100010615     C                   eval      wrkesito = '2'
017200010615     C                   else
017300010615     C                   eval      wrkesito = '6'
017400010615     C                   endif
017500010601     C                   endif
017600010601     C                   endif
017700010531     C                   else
017800010531     C                   eval      wrkesito = '9'
017900010531     C                   endif
018000990915     C*
018100990915     C                   if        %open(tivin00r)
018200990915     C                   close     tivin00r
018300990915     C                   endif
018400010531     C*
018500010531     C                   if        vlrpoi <> 999
018600010531     C                   eval      invfil = vlrpoi
018700010531     C                   endif
018800990921     C*
018900010531     C                   if        invfil > *zeros
019000051108     C                   COMMIT
019100990921     C                   exsr      invio
019200000710     C                   endif
019300010531     C*
019400010612     C                   if        flgGiro = '1'
019500010612     C                   exsr      endela
019600010612     C                   endif
019700010612     C*
019800010531     C                   ENDSR
019900010531     C***
020000010531
020100010531
020200010604
020300010604     C*----------------------------------------------------*
020400010604     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020500010604     C*----------------------------------------------------*
020600010604     C     INZVAR        BEGSR
020700010604     C*
020800010604     C*
020900010604     C                   ENDSR
021000010604     C*----------------------------------------------------*
021100010604     C*  IMPOSTAZIONE CAMPI COSTANTI
021200010604     C*----------------------------------------------------*
021300010604     C     DEFCAM        BEGSR
021400010604     C*
021500010604     C                   CLEAR                   FNVAR000
021600010604     C*
021700010604     C                   ENDSR
021800010604     C*----------------------------------------------------*
021900010604     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAR)
022000010604     C*----------------------------------------------------*
022100010604     C     IMPVAR        BEGSR
022200010604     C*
022300010604     C                   SETOFF                                       3132
022400010604     C                   MOVEL     datcor        VARAAS
022500130108     C                   MOVEL     datcor        VARAGC
022600010604     C*
022700010604     C* Reperimento campi ALFA
022800010604     C                   EVAL      VARDMC=%trim(%subst(vindta:34:50))
022900030219     C*
023000030219     C                   EVAL      PiStr=%trim(%subst(vindta:1:4))
023100030219     C                   EXSR      CHKNUM
023200030219     C                   IF        PiInt=*on
023300030219     C                   Z-ADD     PiVal         VARAAS
023400030219     C                   ELSE
023500030219     C                   SETON                                        31
023600030219     C                   EVAL      vinmsg = %trimr(vinmsg)
023700030219     C                             + ' ' + 'VARAAS'
023800030219     C                   ENDIF
023900010604     C*
024000010604     C                   EVAL      PiStr=%trim(%subst(vindta:5:3))
024100010604     C                   EXSR      CHKNUM
024200010604     C                   IF        PiInt=*on
024300010604     C                   Z-ADD     PiVal         VARLNP
024400010604     C                   ELSE
024500010604     C                   SETON                                        31
024600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
024700010604     C                             + ' ' + 'VARLNP'
024800010604     C                   ENDIF
024900010604     C*
025000010604     C                   EVAL      PiStr=%trim(%subst(vindta:8:2))
025100010604     C                   EXSR      CHKNUM
025200010604     C                   IF        PiInt=*on
025300010604     C                   Z-ADD     PiVal         VARNRS
025400010604     C                   ELSE
025500010604     C                   SETON                                        31
025600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
025700010604     C                             + ' ' + 'VARNRS'
025800010604     C                   ENDIF
025900010604     C*
026000010604     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
026100010604     C                   EXSR      CHKNUM
026200010604     C                   IF        PiInt=*on
026300010604     C                   Z-ADD     PiVal         VARNSP
026400010604     C                   ELSE
026500010604     C                   SETON                                        31
026600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
026700010604     C                             + ' ' + 'VARNSP'
026800010604     C                   ENDIF
026900010604     C*
027000010604     C                   EVAL      PiStr=%trim(%subst(vindta:17:4))
027100010604     C                   EXSR      CHKNUM
027200010604     C                   IF        PiInt=*on
027300010604     C                   Z-ADD     PiVal         VARAGC
027400010604     C                   ELSE
027500010604     C                   SETON                                        31
027600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
027700010604     C                             + ' ' + 'VARAGC'
027800010604     C                   ENDIF
027900010604     C*
028000010604     C                   EVAL      PiStr=%trim(%subst(vindta:21:3))
028100010604     C                   EXSR      CHKNUM
028200010604     C                   IF        PiInt=*on
028300010604     C                   Z-ADD     PiVal         VARFGC
028400010604     C                   ELSE
028500010604     C                   SETON                                        31
028600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
028700010604     C                             + ' ' + 'VARFGC'
028800010604     C                   ENDIF
028900010604     C*
029000010604     C                   EVAL      PiStr=%trim(%subst(vindta:24:7))
029100010604     C                   EXSR      CHKNUM
029200010604     C                   IF        PiInt=*on
029300010604     C                   Z-ADD     PiVal         VARNGC
029400010604     C                   ELSE
029500010604     C                   SETON                                        31
029600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
029700010604     C                             + ' ' + 'VARNGC'
029800010604     C                   ENDIF
029900010604     C*
030000010604     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
030100010604     C                   EXSR      CHKNUM
030200010604     C                   IF        PiInt=*on
030300010604     C                   Z-ADD     PiVal         VARNPR
030400010604     C                   ELSE
030500010604     C                   SETON                                        31
030600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
030700010604     C                             + ' ' + 'VARNPR'
030800010604     C                   ENDIF
030900010604     C*
031000010604     C                   EVAL      PiStr=%trim(%subst(vindta:84:8))
031100010604     C                   EXSR      CHKNUM
031200010604     C                   IF        PiInt=*on
031300010604     C                   Z-ADD     PiVal         VARDTR
031400010604     C                   ELSE
031500010604     C                   SETON                                        31
031600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
031700010604     C                             + ' ' + 'VARDTR'
031800010604     C                   ENDIF
031900010604     C*
032000010604     C                   EVAL      PiStr=%trim(%subst(vindta:92:7))
032100010604     C                   EXSR      CHKNUM
032200010604     C                   IF        PiInt=*on
032300010604     C                   Z-ADD     PiVal         VARCCM
032400010604     C                   ELSE
032500010604     C                   SETON                                        31
032600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
032700010604     C                             + ' ' + 'VARCCM'
032800010604     C                   ENDIF
032900010604     C*
033000010604     C                   EVAL      PiStr=%trim(%subst(vindta:99:8))
033100010604     C                   EXSR      CHKNUM
033200010604     C                   IF        PiInt=*on
033300010604     C                   Z-ADD     PiVal         VARDRD
033400010604     C                   ELSE
033500010604     C                   SETON                                        31
033600010604     C                   EVAL      vinmsg = %trimr(vinmsg)
033700010604     C                             + ' ' + 'VARDRD'
033800010604     C                   ENDIF
033900010604     C*
034000010604     C                   ENDSR
034100010604
034200010604
034300010604
034400010604     C*----------------------------------------------------*
034500010604     C*  CONTROLLO NUMERICITA' CAMPI
034600010604     C*----------------------------------------------------*
034700010604     C     CHKNUM        BEGSR
034800010604     C*
034900010604     C                   call(e)   'ISNUMERIC'
035000010604     C                   PARM                    PiStr            30
035100010604     C                   PARM      '.'           PiDecChr          1
035200010604     C                   PARM      *ZEROS        PiVal            30 9
035300010604     C                   PARM      '0'           PiInt             1
035400010604     C                   PARM      '0'           PiNum             1
035500010604     C                   IF        %error
035600010604     C                   EVAL      PiInt=*off
035700010604     C                   ENDIF
035800010604     C*
035900010604     C                   ENDSR
036000010604     C***
036100010604
036200010604
036300010531
036400010531
036500010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
036600010531     C     repfil        BEGSR
036700010531     C*
036800010531     C                   if        invfil = *zeros and
036900010531     C                             depfil > *zeros and
037000010613     C                             (vinflg = *blanks or
037100010613     C                              vinflg = *zeros)
037200010531     C
037300010531     C                   eval      invfil = depfil
037400010531     C                   endif
037500010531     C*
037600010531     C                   if        depfil <> invfil and
037700010531     C                             invfil > *zeros
037800010531     C                   eval      flgMulti = '1'
037900010531     C                   if        vinflg = *blanks
038000010531     C                   add       1             cntNonEl
038100010531     C                   endif
038200010531     C                   endif
038300010531     C*
038400010531     C                   if        vinflg = '2'
038500010531     C                   eval      flgStato = '2'
038600010531     C                   endif
038700010531     C*
038800010531     C                   ENDSR
038900010531     C***
039000010531
039100010531
039200010531
039300010531
039400990921
039500990921      /TITLE Invio dei dati al punto operativo.
039600990921     C     invio         begsr
039700051017     C*
039800051017     C                   clear                   tibs55ds
039900051017     C                   move      invfil        i50apo
040000051017     C                   call(e)   'TIBS55R'
040100051017     C                   parm                    tibs55ds
040200051017     C*
040300051018     C                   if        %error or
040400051017     C                             o50err <> *blanks
040500051017     C                   eval      wrkesito = '3'
040600051017     C                   else
040700051018     C*
040800051018     C* Imposto i parametri fondamentali: azione e S.I. in cui sottomettere
040900051018     C                   clear                   KPJBA
041000051018     C                   movel     'LG95'        KCOAZ
041100051018     C                   movel     o50asi        KNSIF
041200051018     C*
041300051018     C* Imposto altri parametri affinchè nn si spacchi la sottomissinoe del job
041400051018     C                   movel     'EDPCEDXXX'   KNMUS
041500051018     C                   z-add     *zeros        KNRAZ
041600051018     C                   movel     'DSP01'       KNMTD
041700051018     C                   z-add     *zeros        KNRVJ
041800051018     C                   z-add     *zeros        KNRJO
041900051018     C                   movel     'N'           KSTJO
042000051018     C                   z-add     *zeros        KTMIN
042100051018     C                   movel     'B'           KTPAZ
042200051018     C                   movel     'RICEZ_VAR'   KNMEB
042300051018     C                   z-add     *zeros        KNMSB
042400051018     C                   z-add     *zeros        KKEYF
042500051018     C                   z-add     *zeros        KRCDS
042600051018     C                   movel     'J'           KEXCN
042700051018     C                   movel     'P5'          KCOJB
042800051018     C***                movel     'KEDI'        KQIEB
042900051018     C***                movel     'KEDI'        KQEBI
043000051018     C                   movel     'N'           KCANC
043100051108     C***                call      'BCH10'
043200051108     C***                parm                    KPJBA
043300051108     C                   call      'FNLG95C2'
043400051108     C                   parm                    KPJBA
043500051017     C                   endif
043600990921     C*
043700990921     C                   endsr
043800010531
043900010531
044000010531
044100010531
044200010531      /TITLE Invio dei dati al punto operativo.
044300010531     C     opeini        BEGSR
044400010531     C*
044500010531     C* Inizializzo flag e contatori operativi
044600010531     C                   movel     '0'           flgGiro           1
044700010531     C                   movel     '0'           flgMulti          1
044800010531     C                   movel     '1'           flgStato          1
044900010615     C                   movel     '0'           flgOk             1
045000010531     C                   z-add     *zeros        cntNonEl         10 0
045100010531     C                   z-add     *zeros        depfil            3 0
045200010531     C                   z-add     *zeros        invfil            3 0
045300030407     C*
045400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
045500040311     C                   if        %subst(vlrppt:1:1) = 'S'
045600040311     C                   seton                                        50
045700030407     C                   else
045800040311     C                   setoff                                       50
045900030407     C                   endif
046000010531     C*
046100010531     C                   ENDSR
046200010531     C***
046300051108
046400051108
046500051108
046600051108      /TITLE Gestione errore generico
046700051108     C     *pssr         BEGSR
046800051108     C*
046900051108     C* Se errore => emetto operazione d ROLLBACK
047000051108     C                   ROLBK
047100051108     C*
047200051108     C                   ENDSR     '*CANCL'
047300051108     C***
047400080908
047500080908
047600080908
047700080908      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
047800080908     C     exeBon        BEGSR
047900080908     C*
048000080908     C* Imposto i parametri x il *pgm d "bonifica"
048100080908     C                   EVAL      ElencoChar = *blanks
048200080908     C                   EVAL      TipoElenco = *blanks
048300080908     C                   EVAL      CharSost   = *blanks
048400080908     C                   EVAL      UpperCase  = '1'
048500080908     C                   EVAL      ChkNull    = '1'
048600080908     C                   EVAL      CharNull   = *blanks
048700080908     C*
048800080908     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VARDMC
048900080908     C                   EVAL      TxtInOut   = VARDMC
049000080908     C                   EVAL      EsitoBon   = *blanks
049100080908     C*
049200080908     C                   CALL      'XCHKCHAR'
049300080908     C                   PARM                    TxtInOut
049400080908     C                   PARM                    ElencoChar
049500080908     C                   PARM                    TipoElenco
049600080908     C                   PARM                    CharSost
049700080908     C                   PARM                    UpperCase
049800080908     C                   PARM                    ChkNull
049900080908     C                   PARM                    CharNull
050000080908     C                   PARM                    EsitoBon
050100080908     C*
050200080908     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
050300080908     C                   IF        EsitoBon = '1'
050400080908     C                   EVAL      VARDMC = TxtInOut
050500080908     C                   ENDIF
050600080908     C*
050700080908     C                   ENDSR
050800010531
050900010531
051000990915
051100000613     C     *inzsr        BEGSR
051200990915     C*
051300990915     C     *entry        plist
051400990921     C                   parm                    tivlrds
051500990921     C                   parm      wrkesito      esito
051600000724     C                   parm                    prmlit
051700000710     C                   parm                    prmfir
051800010604     C*
051900010604     C* CALCOLA LA DATA CORRENTE
052000010604     C                   time                    wn14             14 0
052100010604     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
052200010604     C                   z-add     wn8           g08dat
052300010604     C                   z-add     *zeros        g08inv
052400010604     C                   movel     '0'           g08err
052500010604     C                   call      'XSRDA8'
052600010604     C                   parm                    wlbda8
052700010604     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
052800000613     C*
052900000613     C                   ENDSR
053000000613     C***
