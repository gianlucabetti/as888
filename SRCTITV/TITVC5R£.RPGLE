000100040607      /TITLE Upload via Internet: traduzione in EDIVATWR. (x FESTO Cod. 0891154)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600031121     FEDIVATwr  O    E             DISK    usropn
000700040607     Ftitvc5p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040607     Ftitvc5ps  O    f  198        PRINTER usropn
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
003200031121     D wNomeFile       s             30    INZ(*blanks)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100000313     D*
004200010330
004300010330
004400010330
004500990915     C                   time                    wrkdata
004600990915     C                   time                    wrkora
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200020321     C                   exsr      rwvat
005300010601     C*
005400010601     C                   seton                                        lr
005500010601
005600010601
005700010601
005800010601
005900010601     C*--------------------------------------------------------
006000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006100010601     C*--------------------------------------------------------
006200010601     C     PREELA        BEGSR
006300010601     C*
006400010601     C* SE OCCORRE SPEDIRE IN FILIALE
006500010601     C                   if        invfil <> *zeros and
006600010601     C                             flgGiro = '0'
006700010601     C*
006800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006900010601     C                   eval      flgGiro = '1'
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300010601     C     invfil        chain     azorg01l
007400000724     C                   if        %found
007500000724     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000724     C*
007800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000724     C                   PARM                    depcmd
008200000724     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000616     C*
008600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700000616     C                   movel(p)  CMD(2)        depcmd
008800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008900000616     C*
009000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000616     C                   PARM                    depcmd
009400000616     C                   PARM                    LENGH
009500000613     C*
009600040607     C                   if        not %open(titvc5ps)
009700040607     C                   open      titvc5ps
009800000616     C                   except    testdett
009900000613     C                   endif
010000010601     C*
010100010601     C                   endif
010200010601     C*
010300010601     C                   ENDSR
010400010601     C***
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900040607     C                   if        %open(titvc5ps)
012000000616     C                   except    findett
012100040607     C                   close     titvc5ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601
013500010601
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200040607     C                   if        not %open(titvc5p)
014300040607     C                   open      titvc5p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800040607     C                   if        %open(titvc5p)
014900040607     C                   close     titvc5p
015000990914     C                   endif
015100000613     C*
015200000613     C                   ENDSR
015300000613     C***
015400000613
015500010330
015600010601
015700010601
015800010601
015900010330     C*--------------------------------------------------------
016000031121     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
016100010330     C*--------------------------------------------------------
016200020321     C     RWVAT         BEGSR
016300010330     C*
016400010330     C                   if        not %open(tivin00r)
016500010330     C                   open      tivin00r
016600010330     C                   endif
016700031121     C                   if        not %open(edivatwr)
016800031121     C                   open      edivatwr
016900010330     C                   endif
017000010330     C*
017100010604     C                   clear                   §CTROK
017200010604     C                   clear                   §CTRMO
017300010604     C                   clear                   §CTRNO
017400010330     C*
017500010330     C                   DO        *HIVAL
017600010330     C*
017700010330     C                   READ      tivin00r                               70
017800010618     C*
017900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018000010618     C                   if        vinflg = '1'
018100010618     C                   eval      flgOk = '1'
018200010618     C                   endif
018300010618     C*
018400010330     C                   if        vindta > *blanks
018500010330     C                   add       1             rrnum
018600010330     C*
018700010601     C                   if        *in70 = *off and
018800010330     C                             (vinflg = *blanks
018900010330     C                              or vinflg = '0'
019000010330     C                              or vinflg = '2')
019100010330     C*
019200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019300010711     C                   if        vinflg = *blanks or vinflg = '0'
019400010711     C                   clear                   vinmsg
019500010711     C                   endif
019600010601     C*
019700010330     C                   exsr      inzvar
019800010330     C                   exsr      defcam
019900020321     C                   exsr      impvat
020000010601     C*
020100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020200020321     C                   eval      depfil = VATLNP
020300010601     C                   exsr      repfil
020400010601     C                   if        depfil = invfil
020500021025     C                   if        vlrpoi = 999
020600021025     C                   MOVE(P)   invfil        VATFGS
020700021025     C                   else
020800021025     C                   MOVE(P)   vlrpoi        VATFGS
020900021025     C                   endif
021000010601     C*
021100010601     C                   exsr      PREELA
021200010601     C*
021300010604     C* Ebbene...
021400010604     C*
021500010604     C  N31              ADD       1             §CTROK            7 0
021600010604     C   32              ADD       1             §CTRMO            7 0
021700010604     C   31              ADD       1             §CTRNO            7 0
021800020205     C*
021900020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
022000031121     C  N31              WRITE     EDIVAT00
022100010604     C*
022200010604     C                   if        *in31 = *off and
022300010604     C                             *in32 = *off
022400010604     C                   eval      vinflg = '1'
022500010604     C                   else
022600010604     C                   eval      recko = vindta
022700010604     C                   except    rigadett
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010604     C                   endif
023100010601     C*
023200010604     C                   endif
023300010604     C*
023400010330     C                   else
023500010330     C                   eval      vinflg = '1'
023600010330     C                   endif
023700010601     C*
023800010601     C  N70              update    tivin000
023900010330     C*
024000010330     C  N70              ENDdo
024100010601     C
024200010601     C*
024300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024400010601     C                   if        cntNonEl = *zeros or
024500010601     C                             flgMulti = '0'
024600010330     C* Se non ci sono record con errori ...
024700010601     C                   if        §ctrno = 0 and
024800010604     C                             §ctrmo = 0 and
024900010601     C                             flgStato <> '2'
025000010330     C* ... restituisco esito OK.
025100010330     C                   eval      wrkesito = '0'
025200010330     C                   else
025300010330     C                   if        §ctrok > 0
025400010330     C                   eval      wrkesito = '1'
025500010330     C                   else
025600010615     C                   if        flgOk = '0'
025700010615     C                   eval      wrkesito = '2'
025800010615     C                   else
025900010615     C                   eval      wrkesito = '6'
026000010615     C                   endif
026100010330     C                   endif
026200010330     C                   endif
026300010601     C                   else
026400010601     C                   eval      wrkesito = '9'
026500010601     C                   endif
026600010330     C*
026700010330     C                   if        %open(tivin00r)
026800010330     C                   close     tivin00r
026900010330     C                   endif
027000031121     C                   if        %open(edivatwr)
027100031121     C                   close     edivatwr
027200010330     C                   endif
027300010601     C*
027400010601     C                   if        vlrpoi <> 999
027500010601     C                   eval      invfil = vlrpoi
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        §ctrok > 0
027900010601     C                             and invfil > *zeros
028000010330     C                   exsr      invio
028100010330     C                   endif
028200010601     C*
028300010618     C                   if        flgGiro = '1'
028400010601     C                   exsr      endela
028500010618     C                   endif
028600010330     C*
028700010330     C                   ENDSR
028800010330     C***
028900010330
029000010601
029100010601
029200010601
029300010601
029400010330     C*----------------------------------------------------*
029500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029600010330     C*----------------------------------------------------*
029700040607     C     INZVAR        BEGSR
029800010330     C*
029900010330     C*
030000010330     C                   ENDSR
030100010330     C*----------------------------------------------------*
030200010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030300010330     C*----------------------------------------------------*
030400010330     C     DEFCAM        BEGSR
030500010330     C*
030600020204     C* Inizializzo il buffer del record da scrivere
030700031121     C                   CLEAR                   EDIVAT00
030800020204     C* Imposto i valori di default...
030900040607     C                   EVAL      VATCCM = 0891154
031000040607     C                   EVAL      VATLNP = 089
031100020321     C                   EVAL      VATTRC = 'E'
031200020204     C* ... e poi verifico se sono stati passati come parametri
031300020204     C                   IF        vlrppt > *blanks
031400040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020321     C                   Z-ADD     PiVal         VATCCM
031900020204     C                   ENDIF
032000040607     C                   ENDIF
032100040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
032200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032300020204     C                   EXSR      CHKNUM
032400020204     C                   IF        PiInt=*on
032500020321     C                   Z-ADD     PiVal         VATLNP
032600020204     C                   ENDIF
032700040607     C                   ENDIF
032800020321     C                   ENDIF
032900020204     C*
033000010330     C                   ENDSR
033100010607     C*----------------------------------------------------*
033200040607     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
033300010607     C*----------------------------------------------------*
033400020321     C     IMPVAT        BEGSR
033500010607     C*
033600010607     C                   SETOFF                                       3132
033700010607     C*
033800010607     C* Reperimento campi ALFA
033900040607     C                   EVAL      VATNOT=%subst(vindta:26:11)
034000020329     C*
034100020329     C                   MOVEL     DATCOR        VATAAS
034200010607     C*
034300010607     C* Reperimento campi NUMERICI
034400040607     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
034500020304     C                   EXSR      CHKNUM
034600020304     C                   IF        PiInt=*on
034700020321     C                   Z-ADD     PiVal         VATNSP
034800020304     C                   ELSE
034900020304     C                   SETON                                        32
035000020321     C                   Z-ADD     *zeros        VATNSP
035100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
035200020321     C                             + ' ' + 'VATNSP'
035300020304     C                   ENDIF
035400031121     C*
035500031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
035600031121     C                   MOVE(P)   vlrMSG        wNomeFile
035700040607     C                   EVAL      VATCMR = %subst(wNomeFile:3:6)
035800031121     C                   EVAL      VATCNT = 1
035900020204     C*
036000010607     C                   ENDSR
036100010607     C*----------------------------------------------------*
036200010601
036300010330
036400010330     C*----------------------------------------------------*
036500010330     C*  CONTROLLO NUMERICITA' CAMPI
036600010330     C*----------------------------------------------------*
036700010330     C     CHKNUM        BEGSR
036800010330     C*
036900010606     C                   IF        PiDecChr = *blanks
037000010606     C                   EVAL      PiDecChr = ','
037100010606     C                   ENDIF
037200010606     C*
037300010606     C                   CALL(e)   'ISNUMERIC'
037400010330     C                   PARM                    PiStr            30
037500010606     C                   PARM                    PiDecChr          1
037600010330     C                   PARM      *ZEROS        PiVal            30 9
037700010330     C                   PARM      '0'           PiInt             1
037800010330     C                   PARM      '0'           PiNum             1
037900010330     C                   IF        %error
038000010606     C                   EVAL      PiNum=*off
038100010330     C                   ENDIF
038200010330     C*
038300010330     C                   ENDSR
038400010330     C***
038500010330
038600010601
038700010601
038800010601
038900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
039000010601     C     repfil        BEGSR
039100010601     C*
039200010601     C                   if        invfil = *zeros and
039300010601     C                             depfil > *zeros and
039400010629     C                             (vinflg = *blanks or
039500010629     C                              vinflg = *zeros)
039600010601     C
039700010601     C                   eval      invfil = depfil
039800010601     C                   endif
039900010601     C*
040000010601     C                   if        depfil <> invfil and
040100010601     C                             invfil > *zeros
040200010601     C                   eval      flgMulti = '1'
040300010601     C                   if        vinflg = *blanks
040400010601     C                   add       1             cntNonEl
040500010601     C                   endif
040600010601     C                   endif
040700010601     C*
040800010601     C                   if        vinflg = '2'
040900010601     C                   eval      flgStato = '2'
041000010601     C                   endif
041100010601     C*
041200010601     C                   ENDSR
041300010601     C***
041400010601
041500010601
041600010601
041700010330
041800010330
041900010330
042000990920      /TITLE Invio dei dati al punto operativo.
042100000613     C     invio         BEGSR
042200990920     C*
042300990920     C                   reset                   dscmz
042400010601     C                   move      invfil        cmzdst
042500990920     C                   eval      cmzfld = vlrfou
042600990920     C                   eval      cmzmbd = vlrhdl
042700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042800000710     C                   if        prmfir = *blanks
042900031121     C                   eval      cmzfla = 'EDIVAT0F'
043000031121     C                   eval      cmzmba = 'EDIVAT0F'
043100000710     C                   else
043200000710     C                   eval      cmzfla = prmfir
043300000710     C                   eval      cmzmba = prmfir
043400000710     C                   endif
043500990920     C                   eval      cmznrr = *zeros
043600990920     C                   move      §ctrok        cmznrr
043700021018     C                   eval      cmzlba = vlrfl1
043800990920     C                   call(e)   'TIS711C'
043900990920     C                   parm                    dscmz
044000990921     C                   parm      *blanks       esito
044100990920     C                   if        %error
044200990920     C                             or cmzerr = '1'
044300990921     C                             or esito  = '1'
044400000710     C                   eval      wrkesito = '3'
044500990920     C                   endif
044600990920     C*
044700000613     C                   ENDSR
044800990910
044900010601
045000010601
045100010601
045200010601
045300010601      /TITLE Invio dei dati al punto operativo.
045400010601     C     opeini        BEGSR
045500010601     C*
045600010601     C* Inizializzo flag e contatori operativi
045700010601     C                   movel     '0'           flgGiro           1
045800010601     C                   movel     '0'           flgMulti          1
045900010601     C                   movel     '1'           flgStato          1
046000010615     C                   movel     '0'           flgOk             1
046100010601     C                   z-add     *zeros        cntNonEl         10 0
046200010601     C                   z-add     *zeros        depfil            3 0
046300010601     C                   z-add     *zeros        invfil            3 0
046400010601     C*
046500010601     C                   ENDSR
046600010601     C***
046700010601
046800010601
046900010601
047000010330
047100010330
047200000613     C     *inzsr        BEGSR
047300990910     C*
047400990910     C     *entry        plist
047500990920     C                   parm                    tivlrds
047600990921     C                   parm      wrkesito      esito
047700000724     C                   parm                    prmlit
047800000710     C                   parm                    prmfir
047900010330     C*
048000010330     C* CALCOLA LA DATA CORRENTE
048100010330     C                   time                    wn14             14 0
048200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048300010330     C                   z-add     wn8           g08dat
048400010330     C                   z-add     *zeros        g08inv
048500010330     C                   movel     '0'           g08err
048600010330     C                   call      'XSRDA8'
048700010330     C                   parm                    wlbda8
048800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048900000613     C*
049000000613     C                   ENDSR
049100000613     C***
049200990908
049300040607     Otitvc5p   E            riepilogo         2
049400990915     O                                              'Upload via Internet'
049500990915     O                                           +1 'Traduzione TIVIN00R -'
049600031121     O                                           +1 'EDIVATWR'
049700990915     O                                           +1 'Testate bolle'
049800990915     O          E            riepilogo   2
049900990915     O                       wrkdata
050000990915     O                       wrkora              +1
050100990915     O                       procname            +1
050200990915     O          E            riepilogo   2
050300990915     O                                              'Cliente..................:'
050400020321     O                       VATCCM        z     +1
050500990915     O          E            riepilogo   2
050600990920     O                                              'Riferimento Strategi.....:'
050700990920     O                       vlrhdl              +1
050800990915     O          E            riepilogo   2
050900990915     O                                              'Giusti...................:'
051000971022     O                       §CTROK        2   +  1
051100990915     O          E            riepilogo   2
051200990915     O                                              'Sbagliati e corretti.....:'
051300971022     O                       §CTRMO        2   +  1
051400990915     O          E            riepilogo   2
051500990915     O                                              'Sbagliati e scartati.....:'
051600971022     O                       §CTRNO        2   +  1
051700000613
051800040607     Otitvc5ps  E            testdett          2
051900000613     O                                              'Upload via Internet'
052000000613     O                                           +1 'Traduzione TIVIN00R -'
052100031121     O                                           +1 'EDIVATWR'
052200000613     O                                           +1 'Testate bolle'
052300000616     O          E            testdett    3
052400000613     O                                           +2 'N° rec'
052500000613     O                                           +3 'Anteprima contenuto'
052600000616     O          E            rigadett    2
052700000613     O                       rrnum               +2
052800000621     O                       recko               +3
052900000616     O          E            findett     2
053000000613     O                       wrkdata
053100000613     O                       wrkora              +1
053200000613     O                       procname            +1
053300000616     O          E            findett     2
053400000613     O                                              'Cliente..................:'
053500020321     O                       VATCCM        z     +1
053600000616     O          E            findett     2
053700000613     O                                              'Riferimento Strategi.....:'
053800000613     O                       vlrhdl              +1
053900000616     O          E            findett     2
054000000613     O                                              'Giusti...................:'
054100000613     O                       §CTROK        2   +  1
054200000616     O          E            findett     2
054300000613     O                                              'Sbagliati e corretti.....:'
054400000613     O                       §CTRMO        2   +  1
054500000616     O          E            findett     2
054600000613     O                                              'Sbagliati e scartati.....:'
054700000613     O                       §CTRNO        2   +  1
054800000613** CMD - COMANDI CL
054900040607OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVC5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
055000040607OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVC5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
055100040607DLTOVR FILE(TITVC5P TITVC5PS) LVL(*)
055200010606
055300000613
