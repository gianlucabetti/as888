000100051018      /TITLE Upload via Internet: traduzione in FNVAR00R.
000200130108     H DFTACTGRP(*no) ACTGRP(*caller)
000300990915
000400000724     Fazorg01l  if   e           k disk
000500990915     Ftivin00r  uF   E             DISK    usropn
000600051108     FFNVAR00r  O    E             DISK    commit
000700010604     Ftis7t10p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010604     Ftis7t10ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100080908
001200080908     D TxtInOut        S           2048
001300080908     D ElencoChar      S            256
001400080908     D TipoElenco      S              1
001500080908     D CharSost        S              1
001600080908     D UpperCase       S              1
001700080908     D ChkNull         S              1
001800080908     D CharNull        S              1
001900080908     D EsitoBon        S              1
002000080908
002100990915
002200000613     D*------------
002300000613     D* COMANDI
002400000613     D*------------
002500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002600000724     D*------------
002700051017     D tibs55ds      e ds                  inz
002800051017     D kpjba         e ds                  inz
002900990915     D psds           sds
003000990915     D  procname         *PROC
003100990921     D tivlrds       e ds                  extname(tivlr00f)
003200990915     D esito           s              1
003300000724     D prmlit          s             10
003400000710     D prmfir          s             10
003500990921     D wrkesito        s                   like(esito)
003600990915     D wrkdata         s               d
003700990915     D wrkora          s               t
003800011123     D depcmd          s            150    INZ(*blanks)
003900010604     D recko           s            150    INZ(*blanks)
004000010604     D*------------------
004100010604     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200010604     D*------------------
004300010604     D WLBDA8          DS                  INZ
004400010604     D  G08DAT                 1      8  0
004500010604     D  G08INV                 9     16  0
004600010604     D  G08ERR                17     17
004700010604     D  G08TGI                18     22  0
004800010604     D*
004900990915
005000990915     C                   time                    wrkdata
005100990915     C                   time                    wrkora
005200990921     C                   reset                   esito
005300990921     C                   reset                   wrkesito
005400010531     C*
005500010531     C                   exsr      opeini
005600010531     C                   exsr      rwvar
005700010531     C*
005800010531     C                   seton                                        lr
005900010531
006000010531
006100010531
006200010531
006300010531     C*--------------------------------------------------------
006400010531     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010531     C*--------------------------------------------------------
006600010531     C     PREELA        BEGSR
006700010531     C*
006800010531     C* SE OCCORRE SPEDIRE IN FILIALE
006900010531     C                   if        invfil <> *zeros and
007000010531     C                             flgGiro = '0'
007100010531     C*
007200010531     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010531     C                   eval      flgGiro = '1'
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700010531     C     invfil        chain     azorg01l
007800000724     C                   if        %found
007900000724     C                   movel(p)  CMD(1)        depcmd
008000020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000724     C*
008200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011123     C                   Z-ADD     150           LENGH            15 5
008400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000724     C                   PARM                    depcmd
008600000724     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011123     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900990915     C*
010000000613     C* Stampa riepilogo. (IN SEDE)
010100010604     C                   if        not %open(tis7t10ps)
010200010604     C                   open      tis7t10ps
010300000613     C                   endif
010400010531     C*
010500010531     C                   endif
010600010531     C*
010700010531     C                   ENDSR
010800010531     C***
010900010531
011000010531
011100010531
011200010531
011300010531
011400010531
011500010531
011600010531
011700010531
011800010531     C*--------------------------------------------------------
011900010531     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010531     C*--------------------------------------------------------
012100010531     C     ENDELA        BEGSR
012200010531     C*
012300010531     C* Stampa riepilogo. (IN FILIALE)
012400010604     C                   if        not %open(tis7t10p)
012500010604     C                   open      tis7t10p
012600010531     C                   endif
012700010531     C*
012800030407     C   50              except    riepilogo
012900010531     C*
013000010604     C                   if        %open(tis7t10p)
013100010604     C                   close     tis7t10p
013200010531     C                   endif
013300000613     C*
013400010604     C                   if        %open(tis7t10ps)
013500030407     C   50              except    riepsto
013600010604     C                   close     tis7t10ps
013700000613     C                   endif
013800000616     C*
013900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014000011123     C                   Z-ADD     150           LENGH            15 5
014100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014200000616     C                   PARM                    CMD(3)
014300000616     C                   PARM                    LENGH
014400000616     C*
014500010531     C                   ENDSR
014600010531     C***
014700010531
014800010531
014900010531
015000010531
015100990915
015200910830     C*--------------------------------------------------------
015300941229     C* RWVAR   LEGGE FNVARAAR E SCRIVE FNVAR00R              *
015400910830     C*--------------------------------------------------------
015500941229     C     RWVAR         BEGSR
015600990915     C*
015700990915     C                   if        not %open(tivin00r)
015800990915     C                   open      tivin00r
015900990915     C                   endif
016000990915     C*
016100010601     C                   MOVE      *ZERO         §CTROK
016200010601     C                   MOVE      *ZERO         §CTRNO
016300010604     C                   MOVE      *ZERO         §CTRMO
016400921023     C                   DO        *HIVAL
016500010618     C*
016600990915     C                   READ      tivin00r                               70
016700010618     C*
016800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016900010618     C                   if        vinflg = '1'
017000010618     C                   eval      flgOk = '1'
017100010618     C                   endif
017200010618     C*
017300000905     C                   if        vindta > *blanks
017400990915     C*
017500010601     C                   if        *in70 = *off and
017600990915     C                             (vinflg = *blanks
017700010711     C                              or vinflg = '0'
017800010711     C                              or vinflg = '2')
017900990915     C*
018000010531     C                   clear                   FNVAR000
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500010711     C*
018600010604     C                   exsr      inzvar
018700010604     C                   exsr      defcam
018800010604     C                   exsr      impvar
018900010604     C*
019000010604     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019100010604     C                   eval      depfil = VARLNP
019200010604     C                   exsr      repfil
019300010604     C                   if        depfil = invfil
019400010604     C*
019500010604     C                   exsr      PREELA
019600080908     C*
019700080908     C* "bonifico" i dati di input da eventuali caratteri indesiderati
019800080908     C                   exsr      exeBon
019900010604     C*
020000010604     C* Ebbene...
020100010604     C  N31              ADD       1             §CTROK            7 0
020200010604     C   32              ADD       1             §CTRMO            7 0
020300010604     C   31              ADD       1             §CTRNO            7 0
020400010604     C  N31              WRITE     FNVAR000
020500010604     C*
020600010604     C                   if        *in31 = *off and
020700010604     C                             *in32 = *off
020800010604     C                   eval      vinflg = '1'
020900010604     C                   else
021000010604     C                   eval      recko = vindta
021100010612     C                   if        %open(tis7t10ps)
021200030407     C   50              except    rigadett
021300010612     C                   endif
021400010604     C                   eval      vinflg = '2'
021500010604     C                   endif
021600010604     C                   endif
021700010604     C                   endif
021800010604     C*
021900010604     C                   else
022000010604     C                   eval      vinflg = '1'
022100010604     C                   endif
022200000905     C*
022300000905     C  N70              update    tivin000
022400991022     C*
022500991022     C  N70              ENDdo
022600990915     C*
022700010531     C*
022800010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022900010531     C                   if        cntNonEl = *zeros or
023000010531     C                             flgMulti = '0'
023100010601     C* Se non ci sono record con errori ...
023200010601     C                   if        §ctrno = 0 and
023300010604     C                             §ctrmo = 0 and
023400010601     C                             flgStato <> '2'
023500010601     C* ... restituisco esito OK.
023600010601     C                   eval      wrkesito = '0'
023700010601     C                   else
023800010601     C                   if        §ctrok > 0
023900010601     C                   eval      wrkesito = '1'
024000010601     C                   else
024100010615     C                   if        flgOk = '0'
024200010615     C                   eval      wrkesito = '2'
024300010615     C                   else
024400010615     C                   eval      wrkesito = '6'
024500010615     C                   endif
024600010601     C                   endif
024700010601     C                   endif
024800010531     C                   else
024900010531     C                   eval      wrkesito = '9'
025000010531     C                   endif
025100990915     C*
025200990915     C                   if        %open(tivin00r)
025300990915     C                   close     tivin00r
025400990915     C                   endif
025500010531     C*
025600010531     C                   if        vlrpoi <> 999
025700010531     C                   eval      invfil = vlrpoi
025800010531     C                   endif
025900990921     C*
026000010531     C                   if        invfil > *zeros
026100051108     C                   COMMIT
026200990921     C                   exsr      invio
026300000710     C                   endif
026400010531     C*
026500010612     C                   if        flgGiro = '1'
026600010612     C                   exsr      endela
026700010612     C                   endif
026800010612     C*
026900010531     C                   ENDSR
027000010531     C***
027100010531
027200010531
027300010604
027400010604     C*----------------------------------------------------*
027500010604     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027600010604     C*----------------------------------------------------*
027700010604     C     INZVAR        BEGSR
027800010604     C*
027900010604     C*
028000010604     C                   ENDSR
028100010604     C*----------------------------------------------------*
028200010604     C*  IMPOSTAZIONE CAMPI COSTANTI
028300010604     C*----------------------------------------------------*
028400010604     C     DEFCAM        BEGSR
028500010604     C*
028600010604     C                   CLEAR                   FNVAR000
028700010604     C*
028800010604     C                   ENDSR
028900010604     C*----------------------------------------------------*
029000010604     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAR)
029100010604     C*----------------------------------------------------*
029200010604     C     IMPVAR        BEGSR
029300010604     C*
029400010604     C                   SETOFF                                       3132
029500010604     C                   MOVEL     datcor        VARAAS
029600130108     C                   MOVEL     datcor        VARAGC
029700010604     C*
029800010604     C* Reperimento campi ALFA
029900010604     C                   EVAL      VARDMC=%trim(%subst(vindta:34:50))
030000030219     C*
030100030219     C                   EVAL      PiStr=%trim(%subst(vindta:1:4))
030200030219     C                   EXSR      CHKNUM
030300030219     C                   IF        PiInt=*on
030400030219     C                   Z-ADD     PiVal         VARAAS
030500030219     C                   ELSE
030600030219     C                   SETON                                        31
030700030219     C                   EVAL      vinmsg = %trimr(vinmsg)
030800030219     C                             + ' ' + 'VARAAS'
030900030219     C                   ENDIF
031000010604     C*
031100010604     C                   EVAL      PiStr=%trim(%subst(vindta:5:3))
031200010604     C                   EXSR      CHKNUM
031300010604     C                   IF        PiInt=*on
031400010604     C                   Z-ADD     PiVal         VARLNP
031500010604     C                   ELSE
031600010604     C                   SETON                                        31
031700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
031800010604     C                             + ' ' + 'VARLNP'
031900010604     C                   ENDIF
032000010604     C*
032100010604     C                   EVAL      PiStr=%trim(%subst(vindta:8:2))
032200010604     C                   EXSR      CHKNUM
032300010604     C                   IF        PiInt=*on
032400010604     C                   Z-ADD     PiVal         VARNRS
032500010604     C                   ELSE
032600010604     C                   SETON                                        31
032700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
032800010604     C                             + ' ' + 'VARNRS'
032900010604     C                   ENDIF
033000010604     C*
033100010604     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
033200010604     C                   EXSR      CHKNUM
033300010604     C                   IF        PiInt=*on
033400010604     C                   Z-ADD     PiVal         VARNSP
033500010604     C                   ELSE
033600010604     C                   SETON                                        31
033700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
033800010604     C                             + ' ' + 'VARNSP'
033900010604     C                   ENDIF
034000010604     C*
034100010604     C                   EVAL      PiStr=%trim(%subst(vindta:17:4))
034200010604     C                   EXSR      CHKNUM
034300010604     C                   IF        PiInt=*on
034400010604     C                   Z-ADD     PiVal         VARAGC
034500010604     C                   ELSE
034600010604     C                   SETON                                        31
034700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
034800010604     C                             + ' ' + 'VARAGC'
034900010604     C                   ENDIF
035000010604     C*
035100010604     C                   EVAL      PiStr=%trim(%subst(vindta:21:3))
035200010604     C                   EXSR      CHKNUM
035300010604     C                   IF        PiInt=*on
035400010604     C                   Z-ADD     PiVal         VARFGC
035500010604     C                   ELSE
035600010604     C                   SETON                                        31
035700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
035800010604     C                             + ' ' + 'VARFGC'
035900010604     C                   ENDIF
036000010604     C*
036100010604     C                   EVAL      PiStr=%trim(%subst(vindta:24:7))
036200010604     C                   EXSR      CHKNUM
036300010604     C                   IF        PiInt=*on
036400010604     C                   Z-ADD     PiVal         VARNGC
036500010604     C                   ELSE
036600010604     C                   SETON                                        31
036700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
036800010604     C                             + ' ' + 'VARNGC'
036900010604     C                   ENDIF
037000010604     C*
037100010604     C                   EVAL      PiStr=%trim(%subst(vindta:31:3))
037200010604     C                   EXSR      CHKNUM
037300010604     C                   IF        PiInt=*on
037400010604     C                   Z-ADD     PiVal         VARNPR
037500010604     C                   ELSE
037600010604     C                   SETON                                        31
037700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
037800010604     C                             + ' ' + 'VARNPR'
037900010604     C                   ENDIF
038000010604     C*
038100010604     C                   EVAL      PiStr=%trim(%subst(vindta:84:8))
038200010604     C                   EXSR      CHKNUM
038300010604     C                   IF        PiInt=*on
038400010604     C                   Z-ADD     PiVal         VARDTR
038500010604     C                   ELSE
038600010604     C                   SETON                                        31
038700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
038800010604     C                             + ' ' + 'VARDTR'
038900010604     C                   ENDIF
039000010604     C*
039100010604     C                   EVAL      PiStr=%trim(%subst(vindta:92:7))
039200010604     C                   EXSR      CHKNUM
039300010604     C                   IF        PiInt=*on
039400010604     C                   Z-ADD     PiVal         VARCCM
039500010604     C                   ELSE
039600010604     C                   SETON                                        31
039700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010604     C                             + ' ' + 'VARCCM'
039900010604     C                   ENDIF
040000010604     C*
040100010604     C                   EVAL      PiStr=%trim(%subst(vindta:99:8))
040200010604     C                   EXSR      CHKNUM
040300010604     C                   IF        PiInt=*on
040400010604     C                   Z-ADD     PiVal         VARDRD
040500010604     C                   ELSE
040600010604     C                   SETON                                        31
040700010604     C                   EVAL      vinmsg = %trimr(vinmsg)
040800010604     C                             + ' ' + 'VARDRD'
040900010604     C                   ENDIF
041000010604     C*
041100010604     C                   ENDSR
041200010604
041300010604
041400010604
041500010604     C*----------------------------------------------------*
041600010604     C*  CONTROLLO NUMERICITA' CAMPI
041700010604     C*----------------------------------------------------*
041800010604     C     CHKNUM        BEGSR
041900010604     C*
042000010604     C                   call(e)   'ISNUMERIC'
042100010604     C                   PARM                    PiStr            30
042200010604     C                   PARM      '.'           PiDecChr          1
042300010604     C                   PARM      *ZEROS        PiVal            30 9
042400010604     C                   PARM      '0'           PiInt             1
042500010604     C                   PARM      '0'           PiNum             1
042600010604     C                   IF        %error
042700010604     C                   EVAL      PiInt=*off
042800010604     C                   ENDIF
042900010604     C*
043000010604     C                   ENDSR
043100010604     C***
043200010604
043300010604
043400010531
043500010531
043600010531      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
043700010531     C     repfil        BEGSR
043800010531     C*
043900010531     C                   if        invfil = *zeros and
044000010531     C                             depfil > *zeros and
044100010613     C                             (vinflg = *blanks or
044200010613     C                              vinflg = *zeros)
044300010531     C
044400010531     C                   eval      invfil = depfil
044500010531     C                   endif
044600010531     C*
044700010531     C                   if        depfil <> invfil and
044800010531     C                             invfil > *zeros
044900010531     C                   eval      flgMulti = '1'
045000010531     C                   if        vinflg = *blanks
045100010531     C                   add       1             cntNonEl
045200010531     C                   endif
045300010531     C                   endif
045400010531     C*
045500010531     C                   if        vinflg = '2'
045600010531     C                   eval      flgStato = '2'
045700010531     C                   endif
045800010531     C*
045900010531     C                   ENDSR
046000010531     C***
046100010531
046200010531
046300010531
046400010531
046500990921
046600990921      /TITLE Invio dei dati al punto operativo.
046700990921     C     invio         begsr
046800051017     C*
046900051017     C                   clear                   tibs55ds
047000051017     C                   move      invfil        i50apo
047100051017     C                   call(e)   'TIBS55R'
047200051017     C                   parm                    tibs55ds
047300051017     C*
047400051018     C                   if        %error or
047500051017     C                             o50err <> *blanks
047600051017     C                   eval      wrkesito = '3'
047700051017     C                   else
047800051018     C*
047900051018     C* Imposto i parametri fondamentali: azione e S.I. in cui sottomettere
048000051018     C                   clear                   KPJBA
048100051018     C                   movel     'LG95'        KCOAZ
048200051018     C                   movel     o50asi        KNSIF
048300051018     C*
048400051018     C* Imposto altri parametri affinchè nn si spacchi la sottomissinoe del job
048500051018     C                   movel     'EDPCEDXXX'   KNMUS
048600051018     C                   z-add     *zeros        KNRAZ
048700051018     C                   movel     'DSP01'       KNMTD
048800051018     C                   z-add     *zeros        KNRVJ
048900051018     C                   z-add     *zeros        KNRJO
049000051018     C                   movel     'N'           KSTJO
049100051018     C                   z-add     *zeros        KTMIN
049200051018     C                   movel     'B'           KTPAZ
049300051018     C                   movel     'RICEZ_VAR'   KNMEB
049400051018     C                   z-add     *zeros        KNMSB
049500051018     C                   z-add     *zeros        KKEYF
049600051018     C                   z-add     *zeros        KRCDS
049700051018     C                   movel     'J'           KEXCN
049800051018     C                   movel     'P5'          KCOJB
049900051018     C***                movel     'KEDI'        KQIEB
050000051018     C***                movel     'KEDI'        KQEBI
050100051018     C                   movel     'N'           KCANC
050200051108     C***                call      'BCH10'
050300051108     C***                parm                    KPJBA
050400051108     C                   call      'FNLG95C2'
050500051108     C                   parm                    KPJBA
050600051017     C                   endif
050700990921     C*
050800990921     C                   endsr
050900010531
051000010531
051100010531
051200010531
051300010531      /TITLE Invio dei dati al punto operativo.
051400010531     C     opeini        BEGSR
051500010531     C*
051600010531     C* Inizializzo flag e contatori operativi
051700010531     C                   movel     '0'           flgGiro           1
051800010531     C                   movel     '0'           flgMulti          1
051900010531     C                   movel     '1'           flgStato          1
052000010615     C                   movel     '0'           flgOk             1
052100010531     C                   z-add     *zeros        cntNonEl         10 0
052200010531     C                   z-add     *zeros        depfil            3 0
052300010531     C                   z-add     *zeros        invfil            3 0
052400030407     C*
052500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
052600040311     C                   if        %subst(vlrppt:1:1) = 'S'
052700040311     C                   seton                                        50
052800030407     C                   else
052900040311     C                   setoff                                       50
053000030407     C                   endif
053100010531     C*
053200010531     C                   ENDSR
053300010531     C***
053400051108
053500051108
053600051108
053700051108      /TITLE Gestione errore generico
053800051108     C     *pssr         BEGSR
053900051108     C*
054000051108     C* Se errore => emetto operazione d ROLLBACK
054100051108     C                   ROLBK
054200051108     C*
054300051108     C                   ENDSR     '*CANCL'
054400051108     C***
054500080908
054600080908
054700080908
054800080908      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
054900080908     C     exeBon        BEGSR
055000080908     C*
055100080908     C* Imposto i parametri x il *pgm d "bonifica"
055200080908     C                   EVAL      ElencoChar = *blanks
055300080908     C                   EVAL      TipoElenco = *blanks
055400080908     C                   EVAL      CharSost   = *blanks
055500080908     C                   EVAL      UpperCase  = '1'
055600080908     C                   EVAL      ChkNull    = '1'
055700080908     C                   EVAL      CharNull   = *blanks
055800080908     C*
055900080908     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VARDMC
056000080908     C                   EVAL      TxtInOut   = VARDMC
056100080908     C                   EVAL      EsitoBon   = *blanks
056200080908     C*
056300080908     C                   CALL      'XCHKCHAR'
056400080908     C                   PARM                    TxtInOut
056500080908     C                   PARM                    ElencoChar
056600080908     C                   PARM                    TipoElenco
056700080908     C                   PARM                    CharSost
056800080908     C                   PARM                    UpperCase
056900080908     C                   PARM                    ChkNull
057000080908     C                   PARM                    CharNull
057100080908     C                   PARM                    EsitoBon
057200080908     C*
057300080908     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
057400080908     C                   IF        EsitoBon = '1'
057500080908     C                   EVAL      VARDMC = TxtInOut
057600080908     C                   ENDIF
057700080908     C*
057800080908     C                   ENDSR
057900010531
058000010531
058100990915
058200000613     C     *inzsr        BEGSR
058300990915     C*
058400990915     C     *entry        plist
058500990921     C                   parm                    tivlrds
058600990921     C                   parm      wrkesito      esito
058700000724     C                   parm                    prmlit
058800000710     C                   parm                    prmfir
058900010604     C*
059000010604     C* CALCOLA LA DATA CORRENTE
059100010604     C                   time                    wn14             14 0
059200010604     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059300010604     C                   z-add     wn8           g08dat
059400010604     C                   z-add     *zeros        g08inv
059500010604     C                   movel     '0'           g08err
059600010604     C                   call      'XSRDA8'
059700010604     C                   parm                    wlbda8
059800010604     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059900000613     C*
060000000613     C                   ENDSR
060100000613     C***
060200990915
060300010604     Otis7t10p  E            riepilogo         2
060400990915     O                                              'Upload via Internet'
060500990915     O                                           +1 'Traduzione TIVIN00R -'
060600051018     O                                           +1 'FNVAR00R'
060700990915     O                                           +1 'Disposizioni giacenza'
060800990915     O          E            riepilogo   2
060900990915     O                       wrkdata
061000990915     O                       wrkora              +1
061100990915     O                       procname            +1
061200990915     O          E            riepilogo   2
061300990915     O                                              'Cliente..................:'
061400990915     O                       VARCCM        z     +1
061500990915     O          E            riepilogo   2
061600990921     O                                              'Riferimento Strategi.....:'
061700990921     O                       vlrhdl              +1
061800990915     O          E            riepilogo   2
061900990915     O                                              'Giusti...................:'
062000010601     O                       §CTROK        2   +  1
062100000613
062200010604     Otis7t10ps E            riepsto           2
062300000613     O                                              'Upload via Internet'
062400000613     O                                           +1 'Traduzione TIVIN00R -'
062500051018     O                                           +1 'FNVAR00R'
062600000613     O                                           +1 'Disposizioni giacenza'
062700000616     O          E            riepsto     2
062800000613     O                       wrkdata
062900000613     O                       wrkora              +1
063000000613     O                       procname            +1
063100010604     O                                           +3 'Anteprima contenuto'
063200010604     O          E            rigadett    2
063300010604     O                       recko               +3
063400000616     O          E            riepsto     2
063500000613     O                                              'Cliente..................:'
063600000613     O                       VARCCM        z     +1
063700000616     O          E            riepsto     2
063800000613     O                                              'Riferimento Strategi.....:'
063900000613     O                       vlrhdl              +1
064000000616     O          E            riepsto     2
064100000613     O                                              'Giusti...................:'
064200010601     O                       §CTROK        2   +  1
064300000613** CMD - COMANDI CL
064400040310OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T10P)  SAVE(*YES) FORMTYPE(RICCLI) OUTQ(
064500040310OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T10PS) SAVE(*YES) FORMTYPE(RICCLI) OUTQ(
064600010604DLTOVR FILE(TIS7T10P TIS7T10PS) LVL(*)
064700000613
064800000613
