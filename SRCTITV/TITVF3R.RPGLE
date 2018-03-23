000100041019      /TITLE Upload via Internet: traduzione in FIVATWWR. (x RICOH Cod. 0431960)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600041019     FFIVATWWR  O    E             DISK    usropn
000700041018     Ftitvf3p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900041018     Ftitvf3ps  O    f  198        PRINTER usropn
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
005100020321     C                   exsr      rwvat
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
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
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
009500041018     C                   if        not %open(titvf3ps)
009600041018     C                   open      titvf3ps
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
011800041018     C                   if        %open(titvf3ps)
011900000616     C                   except    findett
012000041018     C                   close     titvf3ps
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
014100041018     C                   if        not %open(titvf3p)
014200041018     C                   open      titvf3p
014300990915     C                   endif
014400990915     C*
014500990915     C                   except    riepilogo
014600990915     C*
014700041018     C                   if        %open(titvf3p)
014800041018     C                   close     titvf3p
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
015900041019     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVAT00F              *
016000010330     C*--------------------------------------------------------
016100020321     C     RWVAT         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600041019     C                   if        not %open(fivatwwr)
016700041019     C                   open      fivatwwr
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
019800020321     C                   exsr      impvat
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100020321     C                   eval      depfil = VATLNP
020200010601     C                   exsr      repfil
020300010601     C                   if        depfil = invfil
020400021025     C                   if        vlrpoi = 999
020500021025     C                   MOVE(P)   invfil        VATFGS
020600021025     C                   else
020700021025     C                   MOVE(P)   vlrpoi        VATFGS
020800021025     C                   endif
020900010601     C*
021000010601     C                   exsr      PREELA
021100010601     C*
021200010604     C* Ebbene...
021300010604     C*
021400010604     C  N31              ADD       1             §CTROK            7 0
021500010604     C   32              ADD       1             §CTRMO            7 0
021600010604     C   31              ADD       1             §CTRNO            7 0
021700020205     C*
021800020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
021900041019     C  N31              WRITE     FIVAT000
022000010604     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      recko = vindta
022600010604     C                   except    rigadett
022700010604     C                   eval      vinflg = '2'
022800010604     C                   endif
022900010604     C                   endif
023000010601     C*
023100010604     C                   endif
023200010604     C*
023300010330     C                   else
023400010330     C                   eval      vinflg = '1'
023500010330     C                   endif
023600010601     C*
023700010601     C  N70              update    tivin000
023800010330     C*
023900010330     C  N70              ENDdo
024000010601     C
024100010601     C*
024200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024300010601     C                   if        cntNonEl = *zeros or
024400010601     C                             flgMulti = '0'
024500010330     C* Se non ci sono record con errori ...
024600010601     C                   if        §ctrno = 0 and
024700010604     C                             §ctrmo = 0 and
024800010601     C                             flgStato <> '2'
024900010330     C* ... restituisco esito OK.
025000010330     C                   eval      wrkesito = '0'
025100010330     C                   else
025200010330     C                   if        §ctrok > 0
025300010330     C                   eval      wrkesito = '1'
025400010330     C                   else
025500010615     C                   if        flgOk = '0'
025600010615     C                   eval      wrkesito = '2'
025700010615     C                   else
025800010615     C                   eval      wrkesito = '6'
025900010615     C                   endif
026000010330     C                   endif
026100010330     C                   endif
026200010601     C                   else
026300010601     C                   eval      wrkesito = '9'
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        %open(tivin00r)
026700010330     C                   close     tivin00r
026800010330     C                   endif
026900041019     C                   if        %open(fivatwwr)
027000041019     C                   close     fivatwwr
027100010330     C                   endif
027200010601     C*
027300010601     C                   if        vlrpoi <> 999
027400010601     C                   eval      invfil = vlrpoi
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        §ctrok > 0
027800010601     C                             and invfil > *zeros
027900010330     C                   exsr      invio
028000010330     C                   endif
028100010601     C*
028200010618     C                   if        flgGiro = '1'
028300010601     C                   exsr      endela
028400010618     C                   endif
028500010330     C*
028600010330     C                   ENDSR
028700010330     C***
028800010330
028900010601
029000010601
029100010601
029200010601
029300010330     C*----------------------------------------------------*
029400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500010330     C*----------------------------------------------------*
029600040607     C     INZVAR        BEGSR
029700010330     C*
029800010330     C*
029900010330     C                   ENDSR
030000010330     C*----------------------------------------------------*
030100041018     C*  IMPOSTAZIONE CAMPI COSTANTI
030200010330     C*----------------------------------------------------*
030300010330     C     DEFCAM        BEGSR
030400010330     C*
030500020204     C* Inizializzo il buffer del record da scrivere
030600041019     C                   CLEAR                   FIVAT000
030700020204     C* Imposto i valori di default...
030800041018     C                   EVAL      VATCCM = 0431960
030900041018     C                   EVAL      VATLNP = 043
031000020321     C                   EVAL      VATTRC = 'E'
031100020204     C* ... e poi verifico se sono stati passati come parametri
031200020204     C                   IF        vlrppt > *blanks
031300040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
031400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031500020204     C                   EXSR      CHKNUM
031600020204     C                   IF        PiInt=*on
031700020321     C                   Z-ADD     PiVal         VATCCM
031800020204     C                   ENDIF
031900040607     C                   ENDIF
032000040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020321     C                   Z-ADD     PiVal         VATLNP
032500020204     C                   ENDIF
032600040607     C                   ENDIF
032700020321     C                   ENDIF
032800020204     C*
032900010330     C                   ENDSR
033000010607     C*----------------------------------------------------*
033100041019     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033200010607     C*----------------------------------------------------*
033300020321     C     IMPVAT        BEGSR
033400010607     C*
033500010607     C                   SETOFF                                       3132
033600010607     C*
033700010607     C* Reperimento campi ALFA
033800041021     C                   EVAL      VATNOT=%subst(vindta:28:14)
033900020329     C*
034000020329     C                   MOVEL     DATCOR        VATAAS
034100010607     C*
034200010607     C* Reperimento campi NUMERICI
034300041019     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
034400020304     C                   EXSR      CHKNUM
034500020304     C                   IF        PiInt=*on
034600020321     C                   Z-ADD     PiVal         VATNSP
034700020304     C                   ELSE
034800041018     C                   SETON                                        31
034900020321     C                   Z-ADD     *zeros        VATNSP
035000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
035100020321     C                             + ' ' + 'VATNSP'
035200020304     C                   ENDIF
035300020204     C*
035400010607     C                   ENDSR
035500010607     C*----------------------------------------------------*
035600010601
035700010330
035800010330     C*----------------------------------------------------*
035900010330     C*  CONTROLLO NUMERICITA' CAMPI
036000010330     C*----------------------------------------------------*
036100010330     C     CHKNUM        BEGSR
036200010330     C*
036300010606     C                   IF        PiDecChr = *blanks
036400010606     C                   EVAL      PiDecChr = ','
036500010606     C                   ENDIF
036600010606     C*
036700010606     C                   CALL(e)   'ISNUMERIC'
036800010330     C                   PARM                    PiStr            30
036900010606     C                   PARM                    PiDecChr          1
037000010330     C                   PARM      *ZEROS        PiVal            30 9
037100010330     C                   PARM      '0'           PiInt             1
037200010330     C                   PARM      '0'           PiNum             1
037300010330     C                   IF        %error
037400010606     C                   EVAL      PiNum=*off
037500010330     C                   ENDIF
037600010330     C*
037700010330     C                   ENDSR
037800010330     C***
037900010330
038000010601
038100010601
038200010601
038300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038400010601     C     repfil        BEGSR
038500010601     C*
038600010601     C                   if        invfil = *zeros and
038700010601     C                             depfil > *zeros and
038800010629     C                             (vinflg = *blanks or
038900010629     C                              vinflg = *zeros)
039000010601     C
039100010601     C                   eval      invfil = depfil
039200010601     C                   endif
039300010601     C*
039400010601     C                   if        depfil <> invfil and
039500010601     C                             invfil > *zeros
039600010601     C                   eval      flgMulti = '1'
039700010601     C                   if        vinflg = *blanks
039800010601     C                   add       1             cntNonEl
039900010601     C                   endif
040000010601     C                   endif
040100010601     C*
040200010601     C                   if        vinflg = '2'
040300010601     C                   eval      flgStato = '2'
040400010601     C                   endif
040500010601     C*
040600010601     C                   ENDSR
040700010601     C***
040800010601
040900010601
041000010601
041100010330
041200010330
041300010330
041400990920      /TITLE Invio dei dati al punto operativo.
041500000613     C     invio         BEGSR
041600990920     C*
041700990920     C                   reset                   dscmz
041800010601     C                   move      invfil        cmzdst
041900990920     C                   eval      cmzfld = vlrfou
042000990920     C                   eval      cmzmbd = vlrhdl
042100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042200000710     C                   if        prmfir = *blanks
042300041019     C                   eval      cmzfla = 'FIVAT00F'
042400041019     C                   eval      cmzmba = 'FIVAT00F'
042500000710     C                   else
042600000710     C                   eval      cmzfla = prmfir
042700000710     C                   eval      cmzmba = prmfir
042800000710     C                   endif
042900990920     C                   eval      cmznrr = *zeros
043000990920     C                   move      §ctrok        cmznrr
043100021018     C                   eval      cmzlba = vlrfl1
043200990920     C                   call(e)   'TIS711C'
043300990920     C                   parm                    dscmz
043400990921     C                   parm      *blanks       esito
043500990920     C                   if        %error
043600990920     C                             or cmzerr = '1'
043700990921     C                             or esito  = '1'
043800000710     C                   eval      wrkesito = '3'
043900990920     C                   endif
044000990920     C*
044100000613     C                   ENDSR
044200990910
044300010601
044400010601
044500010601
044600010601
044700010601      /TITLE Invio dei dati al punto operativo.
044800010601     C     opeini        BEGSR
044900010601     C*
045000010601     C* Inizializzo flag e contatori operativi
045100010601     C                   movel     '0'           flgGiro           1
045200010601     C                   movel     '0'           flgMulti          1
045300010601     C                   movel     '1'           flgStato          1
045400010615     C                   movel     '0'           flgOk             1
045500010601     C                   z-add     *zeros        cntNonEl         10 0
045600010601     C                   z-add     *zeros        depfil            3 0
045700010601     C                   z-add     *zeros        invfil            3 0
045800010601     C*
045900010601     C                   ENDSR
046000010601     C***
046100010601
046200010601
046300010601
046400010330
046500010330
046600000613     C     *inzsr        BEGSR
046700990910     C*
046800990910     C     *entry        plist
046900990920     C                   parm                    tivlrds
047000990921     C                   parm      wrkesito      esito
047100000724     C                   parm                    prmlit
047200000710     C                   parm                    prmfir
047300010330     C*
047400010330     C* CALCOLA LA DATA CORRENTE
047500010330     C                   time                    wn14             14 0
047600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
047700010330     C                   z-add     wn8           g08dat
047800010330     C                   z-add     *zeros        g08inv
047900010330     C                   movel     '0'           g08err
048000010330     C                   call      'XSRDA8'
048100010330     C                   parm                    wlbda8
048200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048300000613     C*
048400000613     C                   ENDSR
048500000613     C***
048600990908
048700041018     Otitvf3p   E            riepilogo         2
048800990915     O                                              'Upload via Internet'
048900990915     O                                           +1 'Traduzione TIVIN00R -'
049000041019     O                                           +1 'FIVATWWR'
049100990915     O                                           +1 'Testate bolle'
049200990915     O          E            riepilogo   2
049300990915     O                       wrkdata
049400990915     O                       wrkora              +1
049500990915     O                       procname            +1
049600990915     O          E            riepilogo   2
049700990915     O                                              'Cliente..................:'
049800020321     O                       VATCCM        z     +1
049900990915     O          E            riepilogo   2
050000990920     O                                              'Riferimento Strategi.....:'
050100990920     O                       vlrhdl              +1
050200990915     O          E            riepilogo   2
050300990915     O                                              'Giusti...................:'
050400971022     O                       §CTROK        2   +  1
050500990915     O          E            riepilogo   2
050600990915     O                                              'Sbagliati e corretti.....:'
050700971022     O                       §CTRMO        2   +  1
050800990915     O          E            riepilogo   2
050900990915     O                                              'Sbagliati e scartati.....:'
051000971022     O                       §CTRNO        2   +  1
051100000613
051200041018     Otitvf3ps  E            testdett          2
051300000613     O                                              'Upload via Internet'
051400000613     O                                           +1 'Traduzione TIVIN00R -'
051500041019     O                                           +1 'FIVATWWR'
051600000613     O                                           +1 'Testate bolle'
051700000616     O          E            testdett    3
051800000613     O                                           +2 'N° rec'
051900000613     O                                           +3 'Anteprima contenuto'
052000000616     O          E            rigadett    2
052100000613     O                       rrnum               +2
052200000621     O                       recko               +3
052300000616     O          E            findett     2
052400000613     O                       wrkdata
052500000613     O                       wrkora              +1
052600000613     O                       procname            +1
052700000616     O          E            findett     2
052800000613     O                                              'Cliente..................:'
052900020321     O                       VATCCM        z     +1
053000000616     O          E            findett     2
053100000613     O                                              'Riferimento Strategi.....:'
053200000613     O                       vlrhdl              +1
053300000616     O          E            findett     2
053400000613     O                                              'Giusti...................:'
053500000613     O                       §CTROK        2   +  1
053600000616     O          E            findett     2
053700000613     O                                              'Sbagliati e corretti.....:'
053800000613     O                       §CTRMO        2   +  1
053900000616     O          E            findett     2
054000000613     O                                              'Sbagliati e scartati.....:'
054100000613     O                       §CTRNO        2   +  1
054200000613** CMD - COMANDI CL
054300041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
054400041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
054500041018DLTOVR FILE(TITVF3P TITVF3PS) LVL(*)
054600010606
054700000613
