000100021113      /TITLE Upload via Internet: traduzione in FIVATWWR. (x BENLOG/BENETTON - Cod. 0590586/0590803)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
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
015900021113     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWR              *
016000010330     C*--------------------------------------------------------
016100010607     C     RWVAT         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021113     C                   if        not %open(fivatwwr)
016700021113     C                   open      fivatwwr
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
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700021113     C  N31              WRITE     FIVAT000
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      recko = vindta
022400010604     C                   except    rigadett
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700021113     C                   if        %open(fivatwwr)
026800021113     C                   close     fivatwwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010618     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010330
028700010601
028800010601
028900010601
029000010601
029100010330     C*----------------------------------------------------*
029200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300010330     C*----------------------------------------------------*
029400010330     C     INZVAR        BEGSR
029500010330     C*
029600071220     C                   MOVEL     *blanks       wFlgCCM           1
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400021113     C                   CLEAR                   FIVAT000
030500010607     C                   EVAL      VATLNP = 059
030600010614     C                   EVAL      VATTRC = 'E'
030700071220     C*
030800071220     C* ...verifico se sono stati passati come parametri
030900071220     C                   IF        vlrppt > *blanks
031000071220     C                   EVAL      wFlgCCM=%trim(%subst(vlrppt:1:1))
031100071220     C                   IF        %subst(vlrppt:2:7) <> *blanks
031200071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
031300071220     C                   EXSR      CHKNUM
031400071220     C                   IF        PiInt=*on
031500071220     C                   Z-ADD     PiVal         VATCCM
031600071220     C                   ENDIF
031700071220     C                   ENDIF
031800071220     C                   IF        %subst(vlrppt:9:3) <> *blanks
031900071220     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
032000071220     C                   EXSR      CHKNUM
032100071220     C                   IF        PiInt=*on
032200071220     C                   Z-ADD     PiVal         VATLNP
032300071220     C                   ENDIF
032400071220     C                   ENDIF
032500071220     C                   ENDIF
032600010330     C*
032700010330     C                   ENDSR
032800010607     C*----------------------------------------------------*
032900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033000010607     C*----------------------------------------------------*
033100010607     C     IMPVAT        BEGSR
033200010607     C*
033300010607     C                   SETOFF                                       3132
033400010607     C                   MOVEL     datcor        VATAAS
033500010607     C*
033600010607     C* Reperimento campi ALFA
033700010614     C                   IF        %subst(vindta:5:10) = *blanks
033800010614     C                   SETON                                        32
033900010614     C                   ELSE
034000010614     C                   EVAL      VATNOT = %trim(%subst(vindta:5:10))
034100010614     C                   ENDIF
034200010607     C*
034300010607     C* Reperimento campi NUMERICI
034400010607     C*
034500071220     C* Considerazioni sul cliente mittente (solo se richiesto)
034600071220     C                   IF        wFlgCCM = 'N'
034700040206     C                   IF        %subst(VATNOT:1:1) = '7' or
034800040206     C                             %subst(VATNOT:1:1) = '8'
034900080215     C***                EVAL      VATCCM = 0590803
035000080215     C                   EVAL      VATCCM = 0590586
035100040206     C                   ELSE
035200011218     C                   EVAL      VATCCM = 0590586
035300040206     C                   ENDIF
035400071220     C                   ENDIF
035500010614     C*
035600040531     C***                EVAL      PiStr=%trim(%subst(vindta:30:6)) +
035700040531     C***                                %trim(%subst(vindta:4:1))
035800040531     C                   EVAL      PiStr=%trim(%subst(vindta:77:7))
035900010607     C                   EXSR      CHKNUM
036000010607     C                   IF        PiInt=*on
036100010607     C                   Z-ADD     PiVal         VATNSP
036200010607     C                   ELSE
036300010607     C                   SETON                                        32
036400010607     C                   Z-ADD     *zeros        VATNSP
036500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036600021018     C                             + ' ' + 'VATNSP'
036700010607     C                   ENDIF
036800010607     C*
036900010607     C                   ENDSR
037000010607     C*----------------------------------------------------*
037100010601
037200010330
037300010330
037400010330     C*----------------------------------------------------*
037500010330     C*  CONTROLLO NUMERICITA' CAMPI
037600010330     C*----------------------------------------------------*
037700010330     C     CHKNUM        BEGSR
037800010330     C*
037900010606     C                   IF        PiDecChr = *blanks
038000010606     C                   EVAL      PiDecChr = ','
038100010606     C                   ENDIF
038200010606     C*
038300010606     C                   CALL(e)   'ISNUMERIC'
038400010330     C                   PARM                    PiStr            30
038500010606     C                   PARM                    PiDecChr          1
038600010330     C                   PARM      *ZEROS        PiVal            30 9
038700010330     C                   PARM      '0'           PiInt             1
038800010330     C                   PARM      '0'           PiNum             1
038900010330     C                   IF        %error
039000010606     C                   EVAL      PiNum=*off
039100010330     C                   ENDIF
039200010330     C*
039300010330     C                   ENDSR
039400010330     C***
039500010330
039600010601
039700010601
039800010601
039900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
040000010601     C     repfil        BEGSR
040100010601     C*
040200010601     C                   if        invfil = *zeros and
040300010601     C                             depfil > *zeros and
040400010629     C                             (vinflg = *blanks or
040500010629     C                              vinflg = *zeros)
040600010601     C
040700010601     C                   eval      invfil = depfil
040800010601     C                   endif
040900010601     C*
041000010601     C                   if        depfil <> invfil and
041100010601     C                             invfil > *zeros
041200010601     C                   eval      flgMulti = '1'
041300010601     C                   if        vinflg = *blanks
041400010601     C                   add       1             cntNonEl
041500010601     C                   endif
041600010601     C                   endif
041700010601     C*
041800010601     C                   if        vinflg = '2'
041900010601     C                   eval      flgStato = '2'
042000010601     C                   endif
042100010601     C*
042200010601     C                   ENDSR
042300010601     C***
042400010601
042500010601
042600010601
042700010330
042800010330
042900010330
043000990920      /TITLE Invio dei dati al punto operativo.
043100000613     C     invio         BEGSR
043200990920     C*
043300990920     C                   reset                   dscmz
043400010601     C                   move      invfil        cmzdst
043500990920     C                   eval      cmzfld = vlrfou
043600990920     C                   eval      cmzmbd = vlrhdl
043700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
043800000710     C                   if        prmfir = *blanks
043900021113     C                   eval      cmzfla = 'FIVAT00F'
044000021113     C                   eval      cmzmba = 'FIVAT00F'
044100000710     C                   else
044200000710     C                   eval      cmzfla = prmfir
044300000710     C                   eval      cmzmba = prmfir
044400000710     C                   endif
044500990920     C                   eval      cmznrr = *zeros
044600990920     C                   move      §ctrok        cmznrr
044700021111     C                   eval      cmzlba = vlrfl1
044800990920     C                   call(e)   'TIS711C'
044900990920     C                   parm                    dscmz
045000990921     C                   parm      *blanks       esito
045100990920     C                   if        %error
045200990920     C                             or cmzerr = '1'
045300990921     C                             or esito  = '1'
045400000710     C                   eval      wrkesito = '3'
045500990920     C                   endif
045600990920     C*
045700000613     C                   ENDSR
045800990910
045900010601
046000010601
046100010601
046200010601
046300010601      /TITLE Invio dei dati al punto operativo.
046400010601     C     opeini        BEGSR
046500010601     C*
046600010601     C* Inizializzo flag e contatori operativi
046700010601     C                   movel     '0'           flgGiro           1
046800010601     C                   movel     '0'           flgMulti          1
046900010601     C                   movel     '1'           flgStato          1
047000010615     C                   movel     '0'           flgOk             1
047100010601     C                   z-add     *zeros        cntNonEl         10 0
047200010601     C                   z-add     *zeros        depfil            3 0
047300010601     C                   z-add     *zeros        invfil            3 0
047400010601     C*
047500010601     C                   ENDSR
047600010601     C***
047700010601
047800010601
047900010601
048000010330
048100010330
048200000613     C     *inzsr        BEGSR
048300990910     C*
048400990910     C     *entry        plist
048500990920     C                   parm                    tivlrds
048600990921     C                   parm      wrkesito      esito
048700000724     C                   parm                    prmlit
048800000710     C                   parm                    prmfir
048900010330     C*
049000010330     C* CALCOLA LA DATA CORRENTE
049100010330     C                   time                    wn14             14 0
049200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
049300010330     C                   z-add     wn8           g08dat
049400010330     C                   z-add     *zeros        g08inv
049500010330     C                   movel     '0'           g08err
049600010330     C                   call      'XSRDA8'
049700010330     C                   parm                    wlbda8
049800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049900000613     C*
050000000613     C                   ENDSR
050100000613     C***
050200990908
050300010607     Otis7blp   E            riepilogo         2
050400990915     O                                              'Upload via Internet'
050500990915     O                                           +1 'Traduzione TIVIN00R -'
050600021113     O                                           +1 'FIVATWWR'
050700990915     O                                           +1 'Testate bolle'
050800990915     O          E            riepilogo   2
050900990915     O                       wrkdata
051000990915     O                       wrkora              +1
051100990915     O                       procname            +1
051200990915     O          E            riepilogo   2
051300990915     O                                              'Cliente..................:'
051400010607     O                       VATCCM        z     +1
051500990915     O          E            riepilogo   2
051600990920     O                                              'Riferimento Strategi.....:'
051700990920     O                       vlrhdl              +1
051800990915     O          E            riepilogo   2
051900990915     O                                              'Giusti...................:'
052000971022     O                       §CTROK        2   +  1
052100990915     O          E            riepilogo   2
052200990915     O                                              'Sbagliati e corretti.....:'
052300971022     O                       §CTRMO        2   +  1
052400990915     O          E            riepilogo   2
052500990915     O                                              'Sbagliati e scartati.....:'
052600971022     O                       §CTRNO        2   +  1
052700000613
052800010607     Otis7blps  E            testdett          2
052900000613     O                                              'Upload via Internet'
053000000613     O                                           +1 'Traduzione TIVIN00R -'
053100021113     O                                           +1 'FIVATWWR'
053200000613     O                                           +1 'Testate bolle'
053300000616     O          E            testdett    3
053400000613     O                                           +2 'N° rec'
053500000613     O                                           +3 'Anteprima contenuto'
053600000616     O          E            rigadett    2
053700000613     O                       rrnum               +2
053800000621     O                       recko               +3
053900000616     O          E            findett     2
054000000613     O                       wrkdata
054100000613     O                       wrkora              +1
054200000613     O                       procname            +1
054300000616     O          E            findett     2
054400000613     O                                              'Cliente..................:'
054500010607     O                       VATCCM        z     +1
054600000616     O          E            findett     2
054700000613     O                                              'Riferimento Strategi.....:'
054800000613     O                       vlrhdl              +1
054900000616     O          E            findett     2
055000000613     O                                              'Giusti...................:'
055100000613     O                       §CTROK        2   +  1
055200000616     O          E            findett     2
055300000613     O                                              'Sbagliati e corretti.....:'
055400000613     O                       §CTRMO        2   +  1
055500000616     O          E            findett     2
055600000613     O                                              'Sbagliati e scartati.....:'
055700000613     O                       §CTRNO        2   +  1
055800000613** CMD - COMANDI CL
055900011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
056000011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7BLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
056100010607DLTOVR FILE(TIS7BLP TIS7BLPS) LVL(*)
056200010606
056300000613
