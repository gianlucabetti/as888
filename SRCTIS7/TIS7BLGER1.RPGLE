000100090219      /TITLE Upload via Internet: traduzione in EDIVATWR. (x BENLOG/BENETTON - Cod. 0590586/0590803)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090219     FEDIVATwr  O    E             DISK    usropn
000700010607     Ftis7blp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010607     Ftis7blps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010607     C                   exsr      rwvat
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
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300020829     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500010607     C                   if        not %open(tis7blps)
009600010607     C                   open      tis7blps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011300010601     C*--------------------------------------------------------
011400010601     C     ENDELA        BEGSR
011500990915     C*
011600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011700000613     C*
011800010607     C                   if        %open(tis7blps)
011900000616     C                   except    findett
012000010607     C                   close     tis7blps
012100000613     C                   endif
012200000616     C*
012300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012600000616     C                   PARM                    CMD(3)
012700000616     C                   PARM                    LENGH
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500000613
013600000613     C*--------------------------------------------------------
013700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013800000613     C*--------------------------------------------------------
013900000621     C     STPR          BEGSR
014000000613     C*
014100010607     C                   if        not %open(tis7blp)
014200010607     C                   open      tis7blp
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700010607     C                   if        %open(tis7blp)
014800010607     C                   close     tis7blp
014900990914     C                   endif
015000000613     C*
015100000613     C                   ENDSR
015200000613     C***
015300000613
015400010330
015500010601
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900090219     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWR              *
016000010330     C*--------------------------------------------------------
016100010607     C     RWVAT         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600090219     C                   if        not %open(edivatwr)
016700090219     C                   open      edivatwr
016800010330     C                   endif
016900010330     C*
017000010604     C                   clear                   §CTROK
017100010604     C                   clear                   §CTRMO
017200010604     C                   clear                   §CTRNO
017300010330     C*
017400010330     C                   DO        *HIVAL
017500010330     C*
017600010330     C                   READ      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200010618     C*
018300010330     C                   if        vindta > *blanks
018400010330     C                   add       1             rrnum
018500010330     C*
018600010601     C                   if        *in70 = *off and
018700010330     C                             (vinflg = *blanks
018800010330     C                              or vinflg = '0'
018900010330     C                              or vinflg = '2')
019000010330     C*
019100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019200010711     C                   if        vinflg = *blanks or vinflg = '0'
019300010711     C                   clear                   vinmsg
019400010711     C                   endif
019500010601     C*
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010607     C                   exsr      impvat
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100010607     C                   eval      depfil = VATLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021113     C                   if        vlrpoi = 999
020500021113     C                   MOVE(P)   invfil        VATFGS
020600021113     C                   else
020700021113     C                   MOVE(P)   vlrpoi        VATFGS
020800021113     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100090219     C*
021200090219     C* VALORIZZO CAMPI RELATIVI AL "CMR"
021300090219     C                   move(p)   vlrMSG        wNomeFile        30
021400090219     C                   EVAL      VATCMR = %subst(wNomeFile:1:7)
021500090219     C                   EVAL      VATCNT = 1
021600010601     C*
021700010604     C* Ebbene...
021800010604     C*
021900010604     C  N31              ADD       1             §CTROK            7 0
022000010604     C   32              ADD       1             §CTRMO            7 0
022100010604     C   31              ADD       1             §CTRNO            7 0
022200090219     C  N31              WRITE     EDIVAT00
022300010604     C*
022400010604     C                   if        *in31 = *off and
022500010604     C                             *in32 = *off
022600010604     C                   eval      vinflg = '1'
022700010604     C                   else
022800010604     C                   eval      recko = vindta
022900010604     C                   except    rigadett
023000010604     C                   eval      vinflg = '2'
023100010604     C                   endif
023200010604     C                   endif
023300010601     C*
023400010604     C                   endif
023500010604     C*
023600010330     C                   else
023700010330     C                   eval      vinflg = '1'
023800010330     C                   endif
023900010601     C*
024000010601     C  N70              update    tivin000
024100010330     C*
024200010330     C  N70              ENDdo
024300010601     C
024400010601     C*
024500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024600010601     C                   if        cntNonEl = *zeros or
024700010601     C                             flgMulti = '0'
024800010330     C* Se non ci sono record con errori ...
024900010601     C                   if        §ctrno = 0 and
025000010604     C                             §ctrmo = 0 and
025100010601     C                             flgStato <> '2'
025200010330     C* ... restituisco esito OK.
025300010330     C                   eval      wrkesito = '0'
025400010330     C                   else
025500010330     C                   if        §ctrok > 0
025600010330     C                   eval      wrkesito = '1'
025700010330     C                   else
025800010615     C                   if        flgOk = '0'
025900010615     C                   eval      wrkesito = '2'
026000010615     C                   else
026100010615     C                   eval      wrkesito = '6'
026200010615     C                   endif
026300010330     C                   endif
026400010330     C                   endif
026500010601     C                   else
026600010601     C                   eval      wrkesito = '9'
026700010601     C                   endif
026800010330     C*
026900010330     C                   if        %open(tivin00r)
027000010330     C                   close     tivin00r
027100010330     C                   endif
027200090219     C                   if        %open(edivatwr)
027300090219     C                   close     edivatwr
027400010330     C                   endif
027500010601     C*
027600010601     C                   if        vlrpoi <> 999
027700010601     C                   eval      invfil = vlrpoi
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        §ctrok > 0
028100010601     C                             and invfil > *zeros
028200010330     C                   exsr      invio
028300010330     C                   endif
028400010601     C*
028500010618     C                   if        flgGiro = '1'
028600010618     C                   exsr      endela
028700010618     C                   endif
028800010330     C*
028900010330     C                   ENDSR
029000010330     C***
029100010330
029200010601
029300010601
029400010601
029500010601
029600010330     C*----------------------------------------------------*
029700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029800010330     C*----------------------------------------------------*
029900010330     C     INZVAR        BEGSR
030000010330     C*
030100071220     C                   MOVEL     *blanks       wFlgCCM           1
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900090219     C                   CLEAR                   EDIVAT00
031000010607     C                   EVAL      VATLNP = 059
031100010614     C                   EVAL      VATTRC = 'E'
031200071220     C*
031300071220     C* ...verifico se sono stati passati come parametri
031400071220     C                   IF        vlrppt > *blanks
031500071220     C                   EVAL      wFlgCCM=%trim(%subst(vlrppt:1:1))
031600071220     C                   IF        %subst(vlrppt:2:7) <> *blanks
031700071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
031800071220     C                   EXSR      CHKNUM
031900071220     C                   IF        PiInt=*on
032000071220     C                   Z-ADD     PiVal         VATCCM
032100071220     C                   ENDIF
032200071220     C                   ENDIF
032300071220     C                   IF        %subst(vlrppt:9:3) <> *blanks
032400071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
032500071220     C                   EXSR      CHKNUM
032600071220     C                   IF        PiInt=*on
032700071220     C                   Z-ADD     PiVal         VATLNP
032800071220     C                   ENDIF
032900071220     C                   ENDIF
033000071220     C                   ENDIF
033100010330     C*
033200010330     C                   ENDSR
033300010607     C*----------------------------------------------------*
033400090219     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
033500010607     C*----------------------------------------------------*
033600010607     C     IMPVAT        BEGSR
033700010607     C*
033800010607     C                   SETOFF                                       3132
033900010607     C                   MOVEL     datcor        VATAAS
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010614     C                   IF        %subst(vindta:5:10) = *blanks
034300010614     C                   SETON                                        32
034400010614     C                   ELSE
034500010614     C                   EVAL      VATNOT = %trim(%subst(vindta:5:10))
034600010614     C                   ENDIF
034700010607     C*
034800010607     C* Reperimento campi NUMERICI
034900010607     C*
035000071220     C* Considerazioni sul cliente mittente (solo se richiesto)
035100071220     C                   IF        wFlgCCM = 'N'
035200040206     C                   IF        %subst(VATNOT:1:1) = '7' or
035300040206     C                             %subst(VATNOT:1:1) = '8'
035400080215     C***                EVAL      VATCCM = 0590803
035500080215     C                   EVAL      VATCCM = 0590586
035600040206     C                   ELSE
035700011218     C                   EVAL      VATCCM = 0590586
035800040206     C                   ENDIF
035900071220     C                   ENDIF
036000010614     C*
036100040531     C***                EVAL      PiStr=%trim(%subst(vindta:30:6)) +
036200040531     C***                                %trim(%subst(vindta:4:1))
036300040531     C                   EVAL      PiStr=%trim(%subst(vindta:77:7))
036400010607     C                   EXSR      CHKNUM
036500010607     C                   IF        PiInt=*on
036600010607     C                   Z-ADD     PiVal         VATNSP
036700010607     C                   ELSE
036800010607     C                   SETON                                        32
036900010607     C                   Z-ADD     *zeros        VATNSP
037000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037100021018     C                             + ' ' + 'VATNSP'
037200010607     C                   ENDIF
037300010607     C*
037400010607     C                   ENDSR
037500010607     C*----------------------------------------------------*
037600010601
037700010330
037800010330
037900010330     C*----------------------------------------------------*
038000010330     C*  CONTROLLO NUMERICITA' CAMPI
038100010330     C*----------------------------------------------------*
038200010330     C     CHKNUM        BEGSR
038300010330     C*
038400010606     C                   IF        PiDecChr = *blanks
038500010606     C                   EVAL      PiDecChr = ','
038600010606     C                   ENDIF
038700010606     C*
038800010606     C                   CALL(e)   'ISNUMERIC'
038900010330     C                   PARM                    PiStr            30
039000010606     C                   PARM                    PiDecChr          1
039100010330     C                   PARM      *ZEROS        PiVal            30 9
039200010330     C                   PARM      '0'           PiInt             1
039300010330     C                   PARM      '0'           PiNum             1
039400010330     C                   IF        %error
039500010606     C                   EVAL      PiNum=*off
039600010330     C                   ENDIF
039700010330     C*
039800010330     C                   ENDSR
039900010330     C***
040000010330
040100010601
040200010601
040300010601
040400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040500010601     C     repfil        BEGSR
040600010601     C*
040700010601     C                   if        invfil = *zeros and
040800010601     C                             depfil > *zeros and
040900010629     C                             (vinflg = *blanks or
041000010629     C                              vinflg = *zeros)
041100010601     C
041200010601     C                   eval      invfil = depfil
041300010601     C                   endif
041400010601     C*
041500010601     C                   if        depfil <> invfil and
041600010601     C                             invfil > *zeros
041700010601     C                   eval      flgMulti = '1'
041800010601     C                   if        vinflg = *blanks
041900010601     C                   add       1             cntNonEl
042000010601     C                   endif
042100010601     C                   endif
042200010601     C*
042300010601     C                   if        vinflg = '2'
042400010601     C                   eval      flgStato = '2'
042500010601     C                   endif
042600010601     C*
042700010601     C                   ENDSR
042800010601     C***
042900010601
043000010601
043100010601
043200010330
043300010330
043400010330
043500990920      /TITLE Invio dei dati al punto operativo.
043600000613     C     invio         BEGSR
043700990920     C*
043800990920     C                   reset                   dscmz
043900010601     C                   move      invfil        cmzdst
044000990920     C                   eval      cmzfld = vlrfou
044100990920     C                   eval      cmzmbd = vlrhdl
044200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
044300000710     C                   if        prmfir = *blanks
044400090219     C                   eval      cmzfla = 'EDIVAT0F'
044500090219     C                   eval      cmzmba = 'EDIVAT0F'
044600000710     C                   else
044700000710     C                   eval      cmzfla = prmfir
044800000710     C                   eval      cmzmba = prmfir
044900000710     C                   endif
045000990920     C                   eval      cmznrr = *zeros
045100990920     C                   move      §ctrok        cmznrr
045200021111     C                   eval      cmzlba = vlrfl1
045300990920     C                   call(e)   'TIS711C'
045400990920     C                   parm                    dscmz
045500990921     C                   parm      *blanks       esito
045600990920     C                   if        %error
045700990920     C                             or cmzerr = '1'
045800990921     C                             or esito  = '1'
045900000710     C                   eval      wrkesito = '3'
046000990920     C                   endif
046100990920     C*
046200000613     C                   ENDSR
046300990910
046400010601
046500010601
046600010601
046700010601
046800010601      /TITLE Invio dei dati al punto operativo.
046900010601     C     opeini        BEGSR
047000010601     C*
047100010601     C* Inizializzo flag e contatori operativi
047200010601     C                   movel     '0'           flgGiro           1
047300010601     C                   movel     '0'           flgMulti          1
047400010601     C                   movel     '1'           flgStato          1
047500010615     C                   movel     '0'           flgOk             1
047600010601     C                   z-add     *zeros        cntNonEl         10 0
047700010601     C                   z-add     *zeros        depfil            3 0
047800010601     C                   z-add     *zeros        invfil            3 0
047900010601     C*
048000010601     C                   ENDSR
048100010601     C***
048200010601
048300010601
048400010601
048500010330
048600010330
048700000613     C     *inzsr        BEGSR
048800990910     C*
048900990910     C     *entry        plist
049000990920     C                   parm                    tivlrds
049100990921     C                   parm      wrkesito      esito
049200000724     C                   parm                    prmlit
049300000710     C                   parm                    prmfir
049400010330     C*
049500010330     C* CALCOLA LA DATA CORRENTE
049600010330     C                   time                    wn14             14 0
049700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
049800010330     C                   z-add     wn8           g08dat
049900010330     C                   z-add     *zeros        g08inv
050000010330     C                   movel     '0'           g08err
050100010330     C                   call      'XSRDA8'
050200010330     C                   parm                    wlbda8
050300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
050400000613     C*
050500000613     C                   ENDSR
050600000613     C***
050700990908
050800010607     Otis7blp   E            riepilogo         2
050900990915     O                                              'Upload via Internet'
051000990915     O                                           +1 'Traduzione TIVIN00R -'
051100090219     O                                           +1 'EDIVATWR'
051200990915     O                                           +1 'Testate bolle'
051300990915     O          E            riepilogo   2
051400990915     O                       wrkdata
051500990915     O                       wrkora              +1
051600990915     O                       procname            +1
051700990915     O          E            riepilogo   2
051800990915     O                                              'Cliente..................:'
051900010607     O                       VATCCM        z     +1
052000990915     O          E            riepilogo   2
052100990920     O                                              'Riferimento Strategi.....:'
052200990920     O                       vlrhdl              +1
052300990915     O          E            riepilogo   2
052400990915     O                                              'Giusti...................:'
052500971022     O                       §CTROK        2   +  1
052600990915     O          E            riepilogo   2
052700990915     O                                              'Sbagliati e corretti.....:'
052800971022     O                       §CTRMO        2   +  1
052900990915     O          E            riepilogo   2
053000990915     O                                              'Sbagliati e scartati.....:'
053100971022     O                       §CTRNO        2   +  1
053200000613
053300010607     Otis7blps  E            testdett          2
053400000613     O                                              'Upload via Internet'
053500000613     O                                           +1 'Traduzione TIVIN00R -'
053600090219     O                                           +1 'EDIVATWR'
053700000613     O                                           +1 'Testate bolle'
053800000616     O          E            testdett    3
053900000613     O                                           +2 'N° rec'
054000000613     O                                           +3 'Anteprima contenuto'
054100000616     O          E            rigadett    2
054200000613     O                       rrnum               +2
054300000621     O                       recko               +3
054400000616     O          E            findett     2
054500000613     O                       wrkdata
054600000613     O                       wrkora              +1
054700000613     O                       procname            +1
054800000616     O          E            findett     2
054900000613     O                                              'Cliente..................:'
055000010607     O                       VATCCM        z     +1
055100000616     O          E            findett     2
055200000613     O                                              'Riferimento Strategi.....:'
055300000613     O                       vlrhdl              +1
055400000616     O          E            findett     2
055500000613     O                                              'Giusti...................:'
055600000613     O                       §CTROK        2   +  1
055700000616     O          E            findett     2
055800000613     O                                              'Sbagliati e corretti.....:'
055900000613     O                       §CTRMO        2   +  1
056000000616     O          E            findett     2
056100000613     O                                              'Sbagliati e scartati.....:'
056200000613     O                       §CTRNO        2   +  1
056300000613** CMD - COMANDI CL
056400011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
056500011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
056600010607DLTOVR FILE(TIS7BLP TIS7BLPS) LVL(*)
056700010606
056800000613
